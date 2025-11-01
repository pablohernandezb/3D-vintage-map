#### 1. Library ####

libs <- c(
  "giscoR", "terra", "sf",
  "elevatr", "png", "rayshader"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs],
    dependencies = T
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

#### 2. Getting the shape file ####

venezuela_sf <- giscoR::gisco_get_countries(
  region = c(
    "Americas"
  ),
  resolution = "3"
) |> 
  sf::st_union()

plot(sf::st_geometry(venezuela_sf))

#### 3. The Caribbean and Venezuela ####

caribbean_topo_tif <- terra::rast(
  "caribbean.tiff"
)

terra::plotRGB(caribbean_topo_tif)

#### 4. Caribbean Extent ####

caribbean_bbox <- terra::ext(
  caribbean_topo_tif
) |>
  sf::st_bbox(crs = 3857) |>
  sf::st_as_sfc(crs = 3857) |>
  sf::st_transform(crs = 4326) |>
  sf::st_intersection(venezuela_sf)

plot(sf::st_geometry(caribbean_bbox))

#### 5. Caribbean Elevation ####

caribbean_dem <- elevatr::get_elev_raster(
  locations = caribbean_bbox |> sf::st_as_sf(),
  z = 5, clip = "bbox"
)

caribbean_dem_3857 <- caribbean_dem |>
  terra::rast() |>
  terra::project("EPSG:3857")

terra::plot(caribbean_dem_3857)

#### 6. Resample Old Topo Map ####

caribbean_topo_resampled <- terra::resample(
  x = caribbean_topo_tif,
  y = caribbean_dem_3857,
  method = "bilinear"
)

img_file <- "caribbean_topo_modified.png"

terra::writeRaster(
  caribbean_topo_resampled,
  img_file,
  overwrite = T,
  NAflag = 255
)

caribbean_topo_img <- png::readPNG(
  img_file
)

#### 7. Render Scene ####

h <- nrow(caribbean_dem_3857)
w <- ncol(caribbean_dem_3857)

caribbean_matrix <- rayshader::raster_to_matrix(
  caribbean_dem_3857
)

caribbean_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c("white", "grey80")
    )(128)
  ) |>
  rayshader::add_overlay(
    caribbean_topo_img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    caribbean_matrix,
    zscale = 30,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 2 , h / 2
    ),
    zoom = .42,
    phi = 88,
    theta = 0
  )

rayshader::render_camera(
  zoom = .36
)

#### 8. Render Image ####

rayshader::render_highquality(
  filename = "3d_topo_caribbean.png",
  preview = T,
  interactive = F,
  light = F,
  environment_light = "air_museum_playground_4k.hdr",
  intensity_env = 0.8,
  rotate_env = 90,
  parallel = T,
  samples = 800,
  sample_method = "sobol",
  denoise = TRUE,
  width = w * 2,
  height = h * 2
)