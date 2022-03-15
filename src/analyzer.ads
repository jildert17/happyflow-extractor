with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gcov_Parser;           use Gcov_Parser;
with Langkit_Support.Text;
with DOM.Core;

package Analyzer is

   function Analyze
     (Gcov_Output           : Gcov_Output_Array;
      Project_Path_In       : Unbounded_String;
      Verbose               : Boolean := False;
      No_Coverage_Filtering : Boolean := False)
      return Boolean;

private
   procedure Call_Helper
     (Name       :        Langkit_Support.Text.Text_Type;
      Local_Root :        DOM.Core.Node;
      DOM_Node   : in out DOM.Core.Node);

end Analyzer;
