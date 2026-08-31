# GIS inventory constants ----

inventory_ext <- list(
  raster_primary = c(
    "tif", "tiff",
    "img",
    "ecw",
    "asc", "grd", "dem",
    "rst",
    "sdat",
    "bil", "bip", "bsq"
  ),

  virtual_raster = c("vrt"),

  vector_primary = c(
    "shp", "gpkg",
    "geojson", "json",
    "kml", "kmz",
    "vct",
    "tab",
    "fgb"
  ),

  style_primary = c(
    "qml", "qlr",
    "sld",
    "lyr", "lyrx", "style", "stylx"
  ),

  cad_primary = c("dxf", "dwg", "dgn"),

  tabular_primary = c(
    "csv", "xlsx", "xls", "tsv", "txt", "dbf"
  ),

  document_primary = c(
    "pdf", "docx", "doc", "md", "qmd", "rmd", "html", "htm", "xml"
  ),

  image_primary = c(
    "jpg", "jpeg", "png", "bmp"
  ),

  point_cloud_primary = c(
    "las", "laz"
  ),

  multidim_raster = c(
    "nc", "nc4", "hdf", "h5", "he5"
  ),

  database_primary = c(
    "sqlite", "sqlite3", "db", "geodatabase"
  ),

  tile_package = c(
    "tpk", "tpkx", "vtpk", "mmpk", "mbtiles"
  ),

  archive = c(
    "zip", "7z", "tar", "gz", "tgz", "rar"
  ),

  sidecar = c(
    "shx", "prj", "cpg", "sbn", "sbx", "qpj",
    "aux", "ovr", "rrd", "ige", "tfw", "tifw", "jgw", "wld",
    "shp.xml", "aux.xml", "img.aux.xml",
    "tif.aux.xml", "tif.xml", "tif.vat.dbf", "tif.vat.cpg",
    "tiff.aux.xml", "tiff.xml", "tiff.vat.dbf", "tiff.vat.cpg",
    "ecw.aux.xml", "ecw.xml", "ecw.ovr",
    "gpkg-shm", "gpkg-wal", "gpkg-journal",
    "sqlite-shm", "sqlite-wal", "sqlite-journal",
    "db-shm", "db-wal", "db-journal",
    "rdc", "vdc", "ref", "smp", "avl",
    "sgrd", "mgrd", "sdat.aux.xml", "sdat.ovr",
    "hdr", "stx", "clr",
    "map", "id", "ind",
    "lax", "lasx"
  )
)
