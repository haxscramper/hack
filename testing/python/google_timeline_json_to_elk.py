#!/usr/bin/env python

import json
from datetime import datetime
import xml.etree.ElementTree as ET


def json_to_kml(json_file, output_kml):
    with open(json_file, 'r') as f:
        data = json.load(f)

    # Create KML structure
    kml = ET.Element('kml', xmlns="http://www.opengis.net/kml/2.2")
    document = ET.SubElement(kml, 'Document')

    for segment in data.get('semanticSegments', []):
        placemark = ET.SubElement(document, 'Placemark')
        name = ET.SubElement(placemark, 'name')
        name.text = f"Segment {segment.get('startTime', '')}"

        # TimeSpan for temporal data
        timespan = ET.SubElement(placemark, 'TimeSpan')
        begin = ET.SubElement(timespan, 'begin')
        end = ET.SubElement(timespan, 'end')
        begin.text = segment.get('startTime', '')
        end.text = segment.get('endTime', '')

        # Create LineString for path
        linestring = ET.SubElement(placemark, 'LineString')
        coordinates = ET.SubElement(linestring, 'coordinates')

        coord_list = []
        for point in segment.get('timelinePath', []):
            coords = point['point'].replace('°', '').split(', ')
            lat, lon = coords[0], coords[1]
            coord_list.append(f"{lon},{lat},0")

        coordinates.text = ' '.join(coord_list)

    # Write KML file
    tree = ET.ElementTree(kml)
    tree.write(output_kml, encoding='utf-8', xml_declaration=True)

import sys

# Usage
json_to_kml(sys.argv[1], sys.argv[2])
