------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2015, AdaCore                           --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
--                                                                          --
--  This file is based on:                                                  --
--                                                                          --
--   @file    stm32f4xx_hal_dac.c and stm32f4xx_hal_dac_ex.c                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

-- with Ada.Unchecked_Conversion;

with System;        use System;
with STM32_SVD.CAN; use STM32_SVD.CAN;

package body STM32.CAN is

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This : in out CAN_Controller; Rate : Bit_Rate; Mode : Retransmission_Mode) is
   begin
      Enter_Initialization_Mode (This);
      if Mode = Automatic then
         This.MCR.NART := False;
      else
         This.MCR.NART := True;
      end if;
      This.BTR.SJW := 0;
      case Rate is
         when Rate_50KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 12 - 1;
            This.BTR.BRP := 60 - 1;
         when Rate_100KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 12 - 1;
            This.BTR.BRP := 30 - 1;
         when Rate_125KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 12 - 1;
            This.BTR.BRP := 24 - 1;
         when Rate_250KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 12 - 1;
            This.BTR.BRP := 12 - 1;
         when Rate_500KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 12 - 1;
            This.BTR.BRP := 6 - 1;
         when Rate_1000KBPS =>
            This.BTR.TS2 := 2 - 1;
            This.BTR.TS1 := 7 - 1;
            This.BTR.BRP := 5 - 1;
      end case;

      -- Initialize the CAN filter banks and allocate 14 filters to both
      -- CAN1 and CAN2
      Enable_Filter_Configuration(This);
      if This.FMR.CAN2SB /= 14 then
         This.FMR.CAN2SB := 14;
      end if;
       
      if This'Address = STM32_SVD.CAN1_Base then                                
         null;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         null;
      end if;

      Disable_Filter_Configuration(This);

      Enter_Normal_Mode (This);
      -- FIXME There can be a timeout here it seems...
      -- Maybe start in silent test mode
   end Configure;

   ----------------------
   -- Configure_Filter --
   ----------------------

   procedure Configure_Filter (This : CAN_Controller) is
   begin
      -- 
      if This'Address = STM32_SVD.CAN1_Base then                                
         null;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         null;
      end if;
   end Configure_Filter;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out CAN_Controller; Msg : in Message) is
   begin
      -- FIXME Support all three mailboxes
      This.TDT0R.DLC := Msg.Length;
      This.TI0R.STID := Msg.Standard_Id;
      if Msg.Extended then
         This.TI0R.IDE := True; 
         This.TI0R.EXID := Msg.Extended_Id;
      else
         This.TI0R.IDE := False; 
      end if;
      if Msg.Remote then
         This.TI0R.RTR := True;
      else
         This.TI0R.RTR := False;
      end if;
      -- This.TDT0R.TGT := xxx;
      -- This.TDT0R.TIME := xxx;
      This.TDL0R.Arr(0) := Msg.Data(0);
      This.TDL0R.Arr(1) := Msg.Data(1);
      This.TDL0R.Arr(2) := Msg.Data(2);
      This.TDL0R.Arr(3) := Msg.Data(3);
      This.TDH0R.Arr(4) := Msg.Data(4);
      This.TDH0R.Arr(5) := Msg.Data(5);
      This.TDH0R.Arr(6) := Msg.Data(6);
      This.TDH0R.Arr(7) := Msg.Data(7);
      This.TI0R.TXRQ := True;
      -- TODO Check that it is sent successfully?
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive (This : in out CAN_Controller; Msg : out Message) is
   begin
      -- FIXME Support both mailboxes
      Msg.Length := This.RDT0R.DLC;
      if This.RI0R.IDE then
         Msg.Extended := True;
         Msg.Extended_Id := This.RI0R.EXID;
      else
         Msg.Extended := False;
      end if;
      Msg.Standard_Id := This.RI0R.STID;
      Msg.Remote := This.RI0R.RTR;
      Msg.Data(0) := This.RDL0R.Arr(0);
      Msg.Data(1) := This.RDL0R.Arr(1);
      Msg.Data(2) := This.RDL0R.Arr(2);
      Msg.Data(3) := This.RDL0R.Arr(3);
      Msg.Data(4) := This.RDH0R.Arr(4);
      Msg.Data(5) := This.RDH0R.Arr(5);
      Msg.Data(6) := This.RDH0R.Arr(6);
      Msg.Data(7) := This.RDH0R.Arr(7);
      This.RF0R.RFOM0 := True;
   end Receive;

   -----------------------
   -- Message_Available --
   -----------------------
                                                                                
   function Message_Available (This : CAN_Controller) return Boolean is
      (This.RF0R.FMP0 /= 0 or This.RF1R.FMP1 /= 0);

   ----------------------------
   -- In_Initialization_Mode --
   ----------------------------
                                                                                
   function In_Initialization_Mode (This : CAN_Controller) return Boolean is
      (This.MSR.INAK);

   --------------------
   -- In_Normal_Mode --
   --------------------
                                                                                
   function In_Normal_Mode (This : CAN_Controller) return Boolean is
      (not This.MSR.INAK and not This.MSR.SLAK);

   -------------------
   -- In_Sleep_Mode --
   -------------------
                                                                                
   function In_Sleep_Mode (This : CAN_Controller) return Boolean is
      (This.MSR.SLAK);

   -------------------------------
   -- Enter_Initialization_Mode --
   -------------------------------
                                                                                
   procedure Enter_Initialization_Mode (This : in out CAN_Controller) is
   begin
      This.MCR.INRQ := False;
      if In_Sleep_Mode(This) then
         This.MCR.SLEEP := False;
      end if;
      while not In_Initialization_Mode(This) loop
         null;
      end loop;
   end Enter_Initialization_Mode;

   -----------------------
   -- Enter_Normal_Mode --
   -----------------------
                                                                                
   procedure Enter_Normal_Mode (This : in out CAN_Controller) is
   begin
      while not In_Normal_Mode(This) loop
         null;
      end loop;
   end Enter_Normal_Mode;

   ----------------------
   -- Enter_Sleep_Mode --
   ----------------------
                                                                                
   procedure Enter_Sleep_Mode (This : in out CAN_Controller) is
   begin
      This.MCR.SLEEP := True;
      while not In_Sleep_Mode(This) loop
         null;
      end loop;
   end Enter_Sleep_Mode;

   ----------------------
   -- Enable_Test_Mode --
   ----------------------
                                                                                
   procedure Enable_Test_Mode (This : in out CAN_Controller; Mode : Test_Mode) is
   begin
      case Mode is
         when Silent => This.BTR.SILM := True;
         when Loopback => This.BTR.LBKM := True;
         when Silent_Loopback =>
            This.BTR.LBKM := True;
            This.BTR.SILM := True;
      end case;
   end Enable_Test_Mode;

   -----------------------
   -- Disable_Test_Mode --
   -----------------------
                                                                                
   procedure Disable_Test_Mode (This : in out CAN_Controller; Mode : Test_Mode) is
   begin
      case Mode is
         when Silent => This.BTR.SILM := False;
         when Loopback => This.BTR.LBKM := False;
         when Silent_Loopback =>
            This.BTR.LBKM := False;
            This.BTR.SILM := False;
      end case;
   end Disable_Test_Mode;

   ---------------------------------
   -- Enable_Filter_Configuration --
   ---------------------------------
                                                                                
   procedure Enable_Filter_Configuration (This : in out CAN_Controller) is
   begin
      This.FMR.FINIT := True;
   end Enable_Filter_Configuration;

   ----------------------------------
   -- Disable_Filter_Configuration --
   ----------------------------------
                                                                                
   procedure Disable_Filter_Configuration (This : in out CAN_Controller) is
   begin
      This.FMR.FINIT := False;
   end Disable_Filter_Configuration;

end STM32.CAN;
