# ABS GeoPackages

ABS GeoPackages are suitable for data users who have their own Geospatial Information System (GIS). GeoPackages do not include software.

GeoPackages contain Estimated Resident Population (ERP) data linked together with the ASGS boundaries in an SQLite container that can be easily used. They are formatted in a standards based format gpkg which is in an open format, that is supported by commonly used software systems. 

The ERP GeoPackage contains the following data:
* Geography level - Local Government Areas (LGA) 
* ERP for 30 June 2018
* ERP for Males, Females, Persons 
* Sex ratio
* Median age
* ERP by 5 year age group, from '0-4' to '85 and over', for Males, Females, Persons 

ABS ERP GeoPackages include metadata and reference documents to enable you to use and read the data. The compressed file contains:
* GeoPackage file - Estimated Resident Population data merged with LGA boundary information (.gpkg)
* About_LGA_Geopackage_readme.txt - "Read Me" documentation containing helpful information for users about the GeoPackages and contents (.txt). These files can be viewed in any text editor or web browser.
* Xml file containing geographic specific metadata about the LGA boundaries (.xml)
* Creative Commons Licensing information (.txt)

The population estimates in this product for 2018 are preliminary.


# More information about GeoPackages

The GeoPackage format is a container format specifically designed for holding, viewing and transporting multiple spatial layers and data as a single "file", and may be more suitable for those using a modern GIS system for analysis rather than non-spatial analysis tool such as SAS.

[Open Geospatial Consortium (OGC)](http://www.opengeospatial.org/) GeoPackage is an open, non-proprietary, platform-independent and standards-based data format for geographic information systems implemented as a SQLite database container. Originally designed to meet mobile geospatial needs, it is a way of exchanging geospatial datasets and the associated base maps and layers as a single ready-to-use file.

GeoPackage is based on a technology called [SQLite](https://sqlite.org/), which is a lightweight, public domain database system. If you know SQLite, then you are most of the way to understanding GeoPackage. Most modern geographical information systems and tools (for example MapInfo, ArcMap, QGIS, FME and others) are starting to support GeoPackage natively, making it a widely adopted geospatial data interchange format.

Further information on the specification for GeoPackage can be found on the following website http://www.GeoPackage.org/ , including implementations and examples.

Some advantages of GeoPackage are:
* It is backed by the OGC (http://www.opengeospatial.org/) - the international standards organisation for geospatial information and systems.
* It can hold and transport multiple spatial layers as a self-contained, single file container. 
* It is based on an open and widely supported database format, allowing SQL querying of the data.
* Data can be added and removed from a GeoPackage.
* The file format is vendor agnostic. QGIS, FME and others are starting to support GeoPackage natively, making it a powerful and widely adopted geospatial data interchange format.