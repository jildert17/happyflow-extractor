# happyflow-extractor
Ada Happy-Flow extractor and EMF model generator

1 Introduction
==============

This project is part of my master thesis on Software Engineering.

The happy_flow_filter application is capable of translating Ada code into an intermediate XML representation which can lateron be converted to an Uppaal model.
In order to limit the state space of the eventual model, the application uses coverage information from gcov to only extract these lines of code which have an execution count above a given threshold.

The manual given below is targeted for Windows machines.

2	Installation
==============

2.1	Ada compiler
----------------

Make sure an Ada GCC compiler is installed (e.g. Gnatpro)

2.2	Gcov
--------

Currently Gcov is still part of the AdaCore Gnatpro distribution. However, AdaCore is planning to remove Gcov in favor of Gnatcoverage. Note that Gnatcoverage is not able to give frequencies of line execution, only information whether a line of code is covered or not.

2.3	Gcovr
--------------------------

Gcovr is a Python based tool to generate reports in various formats based on gcov files. One of the supported reports is an XML file in Cobertura format. This XML file is easily used for further processing. The Cobertura format is a widely used formatting to report code coverage.
Cobertura:	http://cobertura.sourceforge.net/ 
Gcovr:		 https://gcovr.com/en/stable/index.html 
Run following command from Admin cmd prompt to install gcovr for all Windows users:
python -m pip install gcovr

2.4	Pycobertura
--------------------------

Pycobertura is a Python library to easily parse XML files in Cobertura format.
For installation for all Windows users, run from Admin cmd prompt:
python -m pip install pycobertura

3	Usage
================

3.1	Code instrumentation
--------------------------

In order to eventually generate coverage reports, the code to be analyzed needs to be instrumented. This is done by adding two compiler flags and one linker flag at the build process:
Compiler flags to add: "-ftest-coverage", "-fprofile-arcs"
Linker flag to add: "-fprofile-generate"

Note that the compilation process adds *.gcno files for each object file

3.2	Happy-Flow
--------------------------

After the code has been instrumented, the application can be used normally to run the machine. One important aspect here is that the software should be shutdown gently in order to give to software the opportunity to store the coverage data. Therefore just pressing Ctrl-C doesn’t work, a C-exit command should be used instead.

The execution of the instrumented application will generate \*.gcda files for each source file, at the same location as the \*.gcno files. Note that the \*.gcda files are updated each time the application is executed, hence cumulative coverage data is generated(!)

3.3	Gcov usage
--------------------------

Gcov is used to generate coverage reports out of the coverage data (\*.gcno and \*.gcda files).

3.3.1	Basic invoke

Invocation is done by calling:

`gcov <path-to-compiled-source-files>\obj\*.gc*`

This invocation results in a \*.gcov file for each source file from the adat_lib

As all gcov files are saved in the root folder, it’s convenient to copy those to a subfolder, e.g.:
  
`move  *.gcov gcov_output/`
  
The *.gcov files look like the original source code files, but now prefixed with a number indicating how many times the line has been executed. Five hashes mean that the line has not been executed, a dash means that there is no coverage information available for this line.

3.4	Gcovr
--------------------------

Gcovr in the background calls Gcov, therefore Gcovr shall be pointed to the \*.gcno and \*.gcda files. Hence it is not necessary to run Gcov separately.
Before running gcovr, make sure to remove all b__\* intermediate files from the object folder

3.4.1	Invoke

To convert all coverage information available in the adat_lib folder, invoke the following command.

`gcovr -o gcovr_output\gcovr_output.txt out\win64\profile\adat_lib\obj\`

To convert the raw coverage data to Cobertura XML output, invoke:

`gcovr -r Source\ -v -o gcovr_output\gcovr_output.xml --xml-pretty out\win64\profile\adat_lib\obj\`

3.5	Pycobertura
--------------------------

Pycobertura can be used to create nicely highlighted html files by running:

`pycobertura show --format html --output gcovr_output\coverage.html gcovr_output_only_attach_pos.xml`

Note that for the above to work the gcovr output xml file should be present in the root, otherwise the tool cannot find the original source files which it needs to create the HTML reports.
However for this ‘Happy-flow model checking’ project, only the Cobertura parsing capabilities of Pycobertura are used.
A python script called parse_gcov.py is created which parses a given ‘gcovr_output.xml’ file and returns a json file with per file all line numbers which are only executed below a certain threshold value, e.g. all lines of code which are executed 8 or less times; and all line numbers which are executed more than 8 times.

3.6	Coverage filter
--------------------------

Tooling is created in the Ada to filter source code based on the retrieved coverage information.
This tool shall be run from root of the original source files, as the first parameter, the path to the Cobertura XML file shall be given e.g.:

`<path-to-source-files>>coverage_filter.exe gcovr_output.xml`

For debugging purposes it is made possible to feed a raw json file directly to the tool (so no Python parsing is done). In order to do so a json file should be given as first argument, as well as the ‘-rawjson’ argument. E.g.: 

`<path-to-source-files>>coverage_filter.exe c:\temp\raw_json.json -rawjson`

3.6.1	Step 1

First this tool calls the above mentioned parse_gcov.py script which parses the Cobertura XML file. The script saves the information shown below as json data to a file, defined as C:\Temp\parsed_cobertura.json
This file is used as a means of a cache. When the -f flag is not given to the tool, and this file is available, then the parse_gcov.py script is not called.

`{"nr_of_files": ##, "line_nr_data": [{"filename": "##", "below_thres": [#, #], "above_thres": [#, #]}]}`

For example:

`{"nr_of_files": 2, "line_nr_data": [{"filename": "source/Applic/Adat/a3_ffc_waferchange-eqc.adb", "below_thres": [13, 15, 21, 22, 24, 26, 27, 29, 30, 32, 33, 34, 35, 36, 37, 38, 39, 40, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 66, 68, 69, 70, 72, 73, 74, 75, 78, 80, 82, 83, 84, 85, 86, 92, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 113, 114, 116, 117, 119, 121, 126, 128, 129, 131, 133, 135, 137, 139, 141, 143, 146, 148, 321, 322, 323, 326, 327, 328, 329, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 348, 349, 351, 354, 355, 356, 357, 359, 360, 362, 364, 365, 370, 372, 373, 374, 375, 377], "above_thres": []},{"filename": "source/general/driver/diagnose/diag-parms.ads", "below_thres": [], "above_thres": [64]}]}`

3.6.2	Step 2

As a second step the output from the Python script is parsed and stored in an array of records where each record holds the filename and set of line numbers which shall be removed and the set of line numbers which is to keep.

3.6.4	Step 3

Step 3 generates an intermediate XML representation of the code, without the constructs not being part of the happy flow.
This step also uses a tree traversal algorithm which goes through each node of the AST. Every node which matches against one of the constructs which should be available in the representation is checked whether it is part of the happy flow or not. If yes the node is added and the traversal jumps into the node. If not, the node is ignored and the algorithm jumps over this node.
Eventually the happy flow representation of each Ada (implementation) file (\*.adb) available in the coverage data is added to one single XML file. As the top node this representation contains a package node containing the name of the package. Furthermore each subprocedure (part of the happy flow) is added including any call statements which have been called. Note that also call statement present in if/elsif/case conditions are treated as such. For each call statement, LibAdaLang is queried to find the location where the target is defined, this information is added to the XML as ‘target_location’ tag.
This step is called by the command shown below:

`<path-to-source-files>>coverage_filter.exe gcovr_output\gcovr_output.xml -f`
