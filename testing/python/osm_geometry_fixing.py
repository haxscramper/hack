#!/usr/bin/env python

import geopandas as gpd
from shapely.geometry import LineString, MultiLineString, Polygon, MultiPolygon

def convert_to_polygon_features():
    def convert_geometry(geometry):
        if isinstance(geometry, MultiPolygon) and len(geometry.geoms) == 1:
            return geometry.geoms[0]
        elif isinstance(geometry, LineString) and geometry.is_ring:
            return Polygon(geometry)
        elif isinstance(geometry, MultiLineString):
            polygons = [Polygon(line) for line in geometry if line.is_ring]
            if len(polygons) == 1:
                return polygons[0]
        return geometry

    gdf = gpd.read_file("/tmp/area_tmp.geojson")
    gdf["geometry"] = gdf["geometry"].apply(convert_geometry)
    gdf = gdf[gdf.geometry.type == "Polygon"]
    # gdf = gdf[["name", "description", "geometry"]]
    gdf.to_file("/tmp/area.geojson", driver="GeoJSON")
    # gdf.to_file('/tmp/area.shp', driver='ESRI Shapefile')

if __name__ == "__main__":
    convert_to_polygon_features()
