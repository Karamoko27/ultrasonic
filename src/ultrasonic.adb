--  STM32-specific package
with STM32.Board; use STM32.Board;
with STM32.GPIO; use STM32.GPIO;
with STM32.Device; use STM32.Device;
with STM32.EXTI; use STM32.EXTI;
with STM32_SVD.GPIO;

--  HAL functions
with HAL.UART; use HAL.UART; 
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;
with HAL.Bitmap; use HAL.Bitmap;
with Screen_Draw;

with HAL.Framebuffer;
with Screen_Draw;

-- to cast imcompatible types
with Ada.Unchecked_Conversion; 

with Ada.Real_Time; use Ada.Real_Time;

procedure ultrasonic is

   --  subtype ultra_pins is GPIO_Point;
   Trig_Pin : STM32.GPIO.GPIO_Point := PC1; -- PC1 for trigger
   Echo_Pin : STM32.GPIO.GPIO_Point := PD0; -- PD0 for ECHO

   --  Configuration records
   Trig_Config : constant STM32.GPIO.GPIO_Port_Configuration:= (
      Mode      => Mode_Out,
      Resistors => Floating, 
      Output_Type => Push_Pull, 
      Speed => Speed_2MHz
   );

   Echo_Config : constant STM32.GPIO.GPIO_Port_Configuration := (
      Mode      => Mode_In,
      Resistors => Pull_Down
   );

   Delay_sec :constant Time_Span := Seconds(1);
   Delay_Us :constant Time_Span := Microseconds(10);    
   trigger_time: Ada.Real_Time.Time;

   procedure Trigger_Sensor is
   begin
      STM32.GPIO.Clear(Trig_Pin); 
      delay until Clock + (Delay_Us/2);
      STM32.GPIO.Set(Trig_Pin);         --  Set TRIG to high
      delay until Clock + Delay_Us;     --  Wait for 10 microseconds
      STM32.GPIO.Clear(Trig_Pin);       --  Set TRIG to low
   end Trigger_Sensor;

   function Echo_Pin_Is_High return Boolean is
      Signal_State : Boolean;
   begin
      Signal_State := STM32.GPIO.Set(Echo_Pin); --  Get the pin state directly
      return Signal_State;
   end Echo_Pin_Is_High;

   function Get_Echo_Time return Ada.Real_Time.Time_Span is
      Start_Time, End_Time : Ada.Real_Time.Time;
   begin
      -- Wait for the pin to go high
      while not Echo_Pin_Is_High loop
         delay until Clock + Microseconds(10);
         Start_Time := Ada.Real_Time.Clock;
      end loop;
      Start_Time := Ada.Real_Time.Clock;

      --Wait for the pin to go low
      while Echo_Pin_Is_High loop
         delay until Clock + Microseconds(10);
         End_Time := Ada.Real_Time.Clock;
      end loop;
      End_Time := Ada.Real_Time.Clock;

      return End_Time - Start_Time;  -- Adjust as needed
   end Get_Echo_Time;

   function Calculate_Distance (Echo_Time : Ada.Real_Time.Time_Span) return Float is
      Speed_Of_Sound : constant Float := 34300.0;  --  in meters per second
      Distance       : Float;
   begin
      --  Convert time to seconds and calculate distance 
      Distance := (Float(Ada.Real_Time.To_Duration(Echo_Time))/2.0) * Speed_Of_Sound ;
      return Distance;
   end Calculate_Distance;


begin

   --  Initialize the GPIO pins
   Configure_IO(Trig_Pin, Trig_Config);  --  PA0 as output
   Configure_IO(Echo_Pin, Echo_Config);  --  PA1 as input with pull-down
   -- Configure_Trigger(Trig_Pin, Event_Falling_Edge, 0);
   -- Configure_Trigger(Echo_Pin, Event_Rising_Edge, 1);

   loop
      Trigger_Sensor;
      declare
         Echo_Time : constant Ada.Real_Time.Time_Span := Get_Echo_Time;
         Distance  : constant Float := Calculate_Distance(Echo_Time);
         Message   : constant String := "Distance: " & Float'Image(Distance) & "cm";
      begin
         --  Output the calculated distance (for now we use yhe STM32 screen to see the outputs)
         --  HAL.UART.Transmit(Message);
         Screen_Draw.WriteMsg(Message);
      end;
      delay until Clock + Delay_sec;  --  Delay 1 second before the next measurement
   end loop;
end ultrasonic;
