with Gcov_Parser; use Gcov_Parser;

package Ast_Filter is

   procedure Filter_Ast (File_Line_Remove : File_Line_Remove_Type;
                         Verbose          : Boolean := False);

end Ast_Filter;
