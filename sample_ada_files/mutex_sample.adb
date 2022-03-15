with Eln.Mutexes;     use Eln.Mutexes;

package body Mutex_Sample is
   
   My_Lock : Mutex := Create_Mutex;
   
   task Task_Sample1;
   task Task_Sample2;

   task body Task_Sample1
   is
   begin
      Lock_Mutex (My_Lock);
      Unlock_Mutex (My_Lock);
   end Task_Sample1;
   
   task body Task_Sample2
   is
   begin
      Lock_Mutex (My_Lock);
      Unlock_Mutex (My_Lock);
   end Task_Sample2;

end Mutex_Sample;

