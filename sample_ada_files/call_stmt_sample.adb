package body Call_Stmt_Sample is

   function Callee return Boolean
   is
   begin
      return True;
   end Callee;

   procedure Caller
   is
      Foo : Boolean;
   begin
      Foo := Callee;
   end Caller;

end Call_Stmt_Sample;
