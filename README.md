# darwincore
> Converting SPI-Birds standard data to annual breeding pairs as a Darwin Core Archive

### Project
NLBIF/SPI-Birds project: Use of long-term individual data on birds as a source of biodiversity information

Project members:
- [Stefan Vriend](https://github.com/StefanVriend) (ORCID: [0000-0002-9006-5988](http://orcid.org/0000-0002-9006-5988))
- [Cherine Jantzen](https://github.com/CherineJ) (ORCID: [0009-0006-0723-2682](http://orcid.org/0009-0006-0723-2682))
- Antica Culina (ORCID: [0000-0003-2910-8085](http://orcid.org/0000-0003-2910-8085))
- Marcel Visser (ORCID: [0000-0002-1456-1939](http://orcid.org/0000-0002-1456-1939))

### Files and functions:
- `create-DwCA.R` is the main file describing the workflow of converting SPI-Birds standard data to a Darwin Core Archive (dwc:Event + dwc:Occurrence) of annual breeding pairs per species per site
- `create-EML.R` is a _tailored_ script to create the metadata (in EML) associated to the Darwin Core Archive for the NIOO dataset
- `map_to_DwC_A()` is a _general_ function that maps SPI-Birds standard data to a Darwin Core Archive (dwc:Event + dwc:Occurrence)
- `calculate_spatial_extent()` is a _general_ function that maps SPI-Birds coordinates to dwc:coordinateUncertaintyInMeters
- `create_meta_xml()` is a _general_ function that creates a structural metadata file that maps the classes (i.e., Event and Occurrence) and terms (i.e., variable names) to their respective IRIs. The function is originates from https://github.com/LTER-LIFE/FDFDT/blob/main/R/create-meta-xml-of-DwCA.R.
