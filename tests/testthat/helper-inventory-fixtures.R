make_inventory_fixture <- function() {
  dt <- data.table::data.table(
    mount = c(
      rep("/mnt/a", 16L),
      "/mnt/b"
    ),
    path = c(
      "roads/roads.shp",
      "roads/roads.shx",
      "roads/roads.dbf",
      "roads/roads.prj",
      "roads/roads.shp.xml",
      "rasters/dem.tif",
      "rasters/dem.tif.aux.xml",
      "rasters/dem.tif.vat.dbf",
      "database/data.gpkg",
      "database/data.gpkg-wal",
      "terrain/grid/hdr.adf",
      "terrain/grid/vat.adf",
      "terrain/grid.ovr",
      "workspace/sample.gdb/a00000001.gdbtable",
      "workspace/sample.gdb/a00000001.gdbtablx",
      "archives/data.zip",
      "roads/roads.shp"
    )
  )

  dt[, name := basename(path)]
  dt[, size := seq_len(.N) * 100]

  dt[, modtime := as.POSIXct(
    "2026-01-01 00:00:00",
    tz = "UTC"
  ) + seq_len(.N)]

  dt[, isdir := FALSE]

  dt[]
}