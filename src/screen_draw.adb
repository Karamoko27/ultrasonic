with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
with BMP_Fonts;

with Bitmapped_Drawing;
with HAL.Framebuffer;

package body Screen_Draw is

   BG : constant Bitmap_Color := (Alpha => 255, others => 64);
   FG : constant Bitmap_Color := (Alpha => 255, others => 255);

   procedure Clear is
   begin
      Display.Hidden_Buffer (1).Set_Source (BG);
      Display.Hidden_Buffer (1).Fill;
      Display.Update_Layer (1, Copy_Back => True);
   end Clear;

   procedure WriteMsg
      (Msg: String)
   is
   begin
      Display.Hidden_Buffer (1).Set_Source (BG);
      Display.Hidden_Buffer (1).Fill;

      Bitmapped_Drawing.Draw_String
         (Display.Hidden_Buffer (1).all, 
         Start => (10, 10),
         Msg => Msg, 
         Font => BMP_Fonts.Font8x8,
         Foreground => FG,
         Background => BG);

      Display.Update_Layer (1, Copy_Back => True);
   end WriteMsg;

begin

   Display.Initialize;
   Display.Set_Orientation (HAL.Framebuffer.Landscape);
   Display.Initialize_Layer (1, ARGB_8888);

end Screen_Draw;