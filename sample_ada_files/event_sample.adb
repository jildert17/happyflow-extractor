with General_Events; use General_Events;

package body Event_Sample is

   Sample_Event : Event := Create_Event;

   task Simple_Task is
      entry Accept_1;
   end Simple_Task;
   
   task body Simple_Task
   is
      Dummy : Boolean;
   begin
      accept Accept_1;
      Dummy := WaitForEvent (Sample_Event);
   end Simple_Task;

   procedure Event_Sample_Function
   is
   begin
     ResetEvent (Sample_Event);
     Simple_Task.Accept_1;
     delay 1.0;
     SetEvent (Sample_Event);
   end Event_Sample_Function;

end Event_Sample;

