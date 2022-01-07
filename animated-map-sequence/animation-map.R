library(tidyverse)
library(geobr)
library(sf)
library(sp)
library(rmapshaper)
library(gganimate)

graphics.off()

# read and simplify map data, read IBGE population data ----
# 
# Projection: UTM code 32723 is for (48W,16S), roughly Brasília

if ( !exists('BR_map') ) {  
  BR_map    = geobr::read_country() %>% st_transform(crs=32723) %>% 
               st_buffer(dist=0)
}  

if ( !exists('UF_map') ) { 
  UF_map = geobr::read_state()   %>% 
               st_transform(crs=32723) %>% 
               mutate_if(is.factor, as.character) %>% 
               ms_simplify(keep=.02) %>% 
               st_buffer(dist=0)
}

if ( !exists('pop') ) { 
  pop       = read_csv('estimated-population-by-state-2020.csv',
                       col_names = c('abbrev_state','name_state','pop2020'),
                       col_types = 'ccd')
}


state_map = UF_map %>% 
   add_column(area = 1e-6*as.numeric(st_area(UF_map))) %>% 
   full_join(pop, by='abbrev_state') %>% 
   select(-name_state.x) %>% 
   rename(name_state = name_state.y) %>% 
   mutate( density = pop2020/area)

region_map = state_map %>% 
              group_by(name_region) %>% 
              summarize(pop2020 = sum(pop2020),
                        area    = sum(area),
                        density = pop2020/area)

# state centroids as an XY matrix
state_centroids =  state_map %>% 
  st_centroid() %>% 
  st_coordinates()

# region centroids as an XY matrix
region_centroids =  region_map %>% 
  st_centroid() %>% 
  st_coordinates()

# region info ----

region_info = tibble(
  name_region = c('Norte', 'Nordeste', 'Sudeste', 'Sul', 'Centro Oeste'),
  code_region = factor(1:5)
)

region_colors = c('#00cc66',  # Norte
                  '#cc66ff',  # Nordeste
                  '#0099ff',  # Sudeste
                  '#ff9500',  # Sul
                  '#f768a1')  # Centro Oeste

# State Polygon Map in XY tibble form ---- 

state_poly = st_coordinates(state_map)

ix = state_poly[,'L2']  # indices 1...27 for states corresp to points in state_poly

state_poly_map = tibble(
  name_region  = state_map$name_region[ix],
  code_state   = state_map$code_state[ix],
  abbrev_state = state_map$abbrev_state[ix],
  X = state_poly[,'X'],
  Y = state_poly[,'Y']
) %>% 
  left_join(region_info)


# Regional Polygon Map in XY tibble form ---- 

region_poly = st_coordinates(region_map)

ix = region_poly[,'L2']  # indices 1..5 for regions corresp to points in region_poly

region_poly_map = tibble(
  name_region  = state_map$name_region[ix],
  X = region_poly[,'X'],
  Y = region_poly[,'Y']
) %>% 
  left_join(region_info)


# calculate the scaling factor for population circles ----

# radius of population circles (in METERS) is scaled by factor K so that
# Area         = K^2 * Pop
# pi * R^2     = K^2 * pop
# sqrt(pi) * R = sqrt(pop) * K
# R            = K * sqrt(pop/pi)
# 
# Choose Rmax, the state_radius of the population circle for the
# most populous state, and calculate the scale factor that generates Rmax

Rmax          = 400e3                     # max state radius in METERS
Pmax          = max(state_map$pop2020)
K             = sqrt(pi/Pmax) * Rmax

state_radius  = K * sqrt( state_map$pop2020  / pi)
region_radius = K * sqrt( region_map$pop2020 / pi)


# function to generate circles in XY tibble form
circle = function(center_x, center_y, radius, n=60) {
  theta = seq(0,2*pi, length.out = n)
  xvals = center_x + radius * cos(theta)
  yvals = center_y + radius * sin(theta)
  tibble(X=xvals, Y=yvals)
}


# state population + circle map in XY tibble form ----

state_circle_map = tibble()

for (i in 1:nrow(state_map)) { 
  tmp = circle( state_centroids[i,'X'],
                state_centroids[i,'Y'],
                state_radius[i]) %>% 
              mutate(name_region  = state_map$name_region[i],
                     code_state   = state_map$code_state[i],
                     abbrev_state = state_map$abbrev_state[i]) %>% 
              select(name_region, code_state, abbrev_state, X, Y) 
              
  state_circle_map = bind_rows(state_circle_map, tmp) 
}

state_circle_map = state_circle_map  %>% 
         left_join(region_info)

# regional population + circle map in XY tibble form ----

region_circle_map = tibble()

for (i in 1:nrow(region_map)) { 
  tmp = circle( region_centroids[i,'X'],
                region_centroids[i,'Y'],
                region_radius[i]) %>% 
        mutate(name_region  =  region_map$name_region[i]) %>% 
        select(name_region, X, Y) 
    
  region_circle_map = bind_rows(region_circle_map, tmp) 
}

region_circle_map = region_circle_map  %>% 
  mutate(abbrev_state=NA, code_state=NA) %>% 
  left_join(region_info) 

# construct dfs for labels that include population data ----

state_labels = (state_map$pop2020 /1e6) %>% 
  sprintf("%.1f",.) %>% 
  paste0(state_map$abbrev_state," ",.,'M')

region_labels = (region_map$pop2020 /1e6) %>% 
  sprintf("%.1f",.) %>% 
  paste0(region_map$name_region," ",.,'M')

national_label_info = region_centroids %>% 
                        as_tibble() %>% 
                        rename(label.X = X, label.Y = Y) %>% 
                        add_column(text     =region_map$name_region)

region_label_info = region_centroids %>% 
                     as_tibble() %>% 
                     rename(label.X = X, label.Y = Y) %>% 
                     add_column(text     = region_labels)

state_label_info = state_centroids %>% 
  as_tibble() %>% 
  rename(centroid.X = X, centroid.Y = Y) %>% 
  add_column(text         = state_labels,
             abbrev_state = state_map$abbrev_state)

# manual label offsets from state centroids, to improve map readability
# (enter data in KM; it will be converted to METERS)
state_label_offsets = tribble(
  ~abbrev_state, ~offset.X, ~offset.Y,
  "RO",0,-200,
  "AC",0,-200,
  "AM",0,-250,
  "RR",0,-200,
  "PA",0,-250,
  "AP",0,-200,
  "TO",0,-200,
  "MA",0,+300,
  "PI",0,-250,
  "CE",0, +300,
  "RN", +200,+250,
  "PB", +400,+100,
  "PE", +550,0,
  "AL", +350,-100,
  "SE",+100,-200,
  "BA",+400,-300,
  "MG",+500,+100,
  "ES",+300,-150,
  "RJ",+450,-150,
  "SP",+450,-400,
  "PR",-300,-150,
  "SC",+300,-150,
  "RS",+300,-150,
  "MS",0,-200,
  "MT",0,-250,
  "GO",-300,-150,
  "DF",0,+200
)  %>% 
  mutate(offset.X = 1000 * offset.X,
         offset.Y = 1000 * offset.Y)

# move the state labels to (centroid + offset)
state_label_info = state_label_info %>% 
                    left_join(state_label_offsets, by='abbrev_state') %>% 
                    mutate(label.X = centroid.X + offset.X,
                           label.Y = centroid.Y + offset.Y)


# stack maps and assemble animation ----

# stack the XY tibbles. Add an identifier for the map type,
# and a common variable to describe the map's main geographical unit

map_stack = bind_rows(
           state_poly_map    %>% mutate(map_type = '1state_polygons', unit=abbrev_state),
           region_circle_map %>% mutate(map_type = '2region_circles', unit=name_region),
           state_circle_map  %>% mutate(map_type = '3state_circles',  unit=abbrev_state),
) %>% 
  left_join(region_info)


label_stack = bind_rows(
  national_label_info   %>% mutate(map_type = '1state_polygons'),
  region_label_info     %>% mutate(map_type = '2region_circles'),
  state_label_info      %>% mutate(map_type = '3state_circles')
)

# construct and save the animation ----

BR = 
  ggplot(data=map_stack) +
  aes(x=X, y=Y, frame=map_type, group=unit, fill=code_region) +
  geom_polygon(alpha=.75) +
  geom_path() +
  geom_sf(data=state_map, fill=NA, inherit.aes = FALSE) +
  guides(fill='none') +
  coord_sf() +
  theme_void() +
  theme(plot.title   = element_text(hjust = 0.5, size=18, face='bold'),
        plot.caption = element_text(size=9)) +
  transition_states(map_type, wrap=TRUE,
                    state_length=c(1,6,8), 
                    transition_length = 2) +
  exit_fade() +
  enter_grow() +
  labs(title='Brasil: Populações Regionais e Estaduais, 2020',
       caption= "Fonte: https://ftp.ibge.gov.br/Estimativas_de_Populacao/Estimativas_2020/POP2020_20211117.xls") +
  scale_fill_manual(values=region_colors) +
  geom_label(data=label_stack, aes(x=label.X, y=label.Y, label=text), 
             inherit.aes = FALSE, alpha=.60)


animated <- animate(BR, fps = 5, duration = 20, 
                    start_pause = 5, end_pause = 5, 
                    rewind = FALSE)

anim_save(filename = "animated-map.gif", animated)


