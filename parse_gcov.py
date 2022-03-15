#!/usr/bin/env python

import sys
import json
from pycobertura import Cobertura

def get_line_hits_below_hit_number (file, hit_number):
    el = cobertura_obj._get_class_element_by_filename(file)
    lines = el.xpath("./lines/line[@hits<="+hit_number+"]")
    return [int(line.attrib["number"]) for line in lines]

def get_line_hits_above_hit_number (file, hit_number):
    el = cobertura_obj._get_class_element_by_filename(file)
    lines = el.xpath("./lines/line[@hits>"+hit_number+"]")
    return [int(line.attrib["number"]) for line in lines]
    
# when there is a source element present in the Cobertura file, strip the parts before the source path from the filename such that we get a relative path
# E.g., Input: "C:/Projects/My_Project/Src/my_file.adb"
#       Source definition in Cobertura: "Src\"
#       Output: "Src/my_file.adb"
def get_relative_path (file):
    source = cobertura_obj.xml.xpath("//sources/source[1]/text()")[0]
    
    if source != "":
        source = source.replace ('\\','')
        index = file.lower().find (source.lower())
        return file[index:]
    else:
        return file

if __name__ == '__main__':
    input_file = sys.argv[1]
    hit_number = sys.argv[2]
    
    cobertura_obj = Cobertura(input_file)
    
    gcov_line_data = {"nr_of_files" : len(cobertura_obj.files()),
                      "line_nr_data" : []}
    
    for file in cobertura_obj.files():       
        gcov_line_data["line_nr_data"].append ({"filename" : get_relative_path (file),
                                                "below_thres" : get_line_hits_below_hit_number (file, hit_number),
                                                "above_thres" : get_line_hits_above_hit_number (file, hit_number)})

    f = open ("C:/temp/parsed_cobertura.json", "w")
    json.dump(gcov_line_data, f)
    f.close