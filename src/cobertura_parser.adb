with Python;

package body cobertura_parser is
   
   procedure Parse_Cobertura (Input_File : String) is
      Cobertura : Python.Module;
      Gcov_Parser : Python.Module;
   begin
      
      Python.Initialize;

      Cobertura := Python.Import_File ("pycobertura");
      Python.Call (Cobertura, Input_File);
   
      Gcov_Parser := Python.Import_File ("happy_flow_filter");
      Python.Call (Gcov_Parser, "parse_cobertura_file", Input_File);
   end Parse_Cobertura;
      
end cobertura_parser;
