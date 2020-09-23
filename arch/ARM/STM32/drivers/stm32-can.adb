------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2020, AdaCore and other contributors    --
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
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

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
      -- CAN1 and CAN2 (FIXME Make the partitioning of filters configurable)
      Enable_Filter_Configuration(This);

      if This.FMR.CAN2SB /= 14 then
         This.FMR.CAN2SB := 14;
      end if;
       
      Disable_All_Filters(This);

      Disable_Filter_Configuration(This);

      Enable_Test_Mode(This, Silent);
   end Configure;

   -------------------
   -- Enable_Filter --
   -------------------

   procedure Enable_Filter (This : in out CAN_Controller; Filter : in Integer) is
   begin
      -- CAN1 and CAN2 share a pool of 28 filters, the first This.FMR.CAN2SB
      -- entries belong to CAN1, the remaining entries belong to CAN2
      if This'Address = STM32_SVD.CAN1_Base then
         This.FA1R.FACT.Arr(Filter) := True;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         This.FA1R.FACT.Arr(Filter + Integer(This.FMR.CAN2SB)) := True;
      end if;
   end Enable_Filter;

   --------------------
   -- Disable_Filter --
   --------------------

   procedure Disable_Filter (This : in out CAN_Controller; Filter : in Integer) is
   begin
      -- CAN1 and CAN2 share a pool of 28 filters, the first This.FMR.CAN2SB
      -- entries belong to CAN1, the remaining entries belong to CAN2
      if This'Address = STM32_SVD.CAN1_Base then
         This.FA1R.FACT.Arr(Filter) := False;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         This.FA1R.FACT.Arr(Filter + Integer(This.FMR.CAN2SB)) := False;
      end if;
   end Disable_Filter;

   -------------------------
   -- Disable_All_Filters --
   -------------------------

   procedure Disable_All_Filters (This : in out CAN_Controller) is
   begin
      -- CAN1 and CAN2 share a pool of 28 filters, the first This.FMR.CAN2SB
      -- entries belong to CAN1, the remaining entries belong to CAN2
      if This'Address = STM32_SVD.CAN1_Base then                                
         for Filter in 0 .. Integer(This.FMR.CAN2SB - 1) loop
            This.FA1R.FACT.Arr(Filter) := False;
         end loop;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         for Filter in Integer(This.FMR.CAN2SB) .. 27 loop
            This.FA1R.FACT.Arr(Filter) := False;
         end loop;
      end if;
   end Disable_All_Filters;

   ----------------------
   -- Configure_Filter --
   ----------------------

   function To_UInt32 is new Ada.Unchecked_Conversion(Long_Filter, UInt32);

   function To_UInt32 is new Ada.Unchecked_Conversion(Short_Filter, UInt32);

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               Scale  : in Filter_Scale;
                               W1     : in UInt32;
                               W2     : in UInt32) is
      Offset : Integer;
   begin
      Disable_Filter (This, Filter);

      -- CAN1 and CAN2 share a pool of 28 filters, the first This.FMR.CAN2SB
      -- entries belong to CAN1, the remaining entries belong to CAN2
      if This'Address = STM32_SVD.CAN1_Base then                                
         -- Filters for CAN1 start at 0
         Offset := 0;
      elsif This'Address = STM32_SVD.CAN2_Base then                             
         -- Filters for CAN2 start at This.FMR.CAN2SB
         Offset := Integer(This.FMR.CAN2SB);
      end if;

      -- Write Id/Mask to the filter bank
      CAN12_Filters(2*(Filter + Offset))     := W1;
      CAN12_Filters(2*(Filter + Offset) + 1) := W2;

      -- Configure the Mode, Scale and Fifo for this filter
      case Mode is
         when Mask_Mode =>
            This.FM1R.FBM.Arr(Filter + Offset) := False;
         when List_Mode =>
            This.FM1R.FBM.Arr(Filter + Offset) := True;
      end case;
      case Scale is
         when Dual_Scale =>
            This.FS1R.FSC.Arr(Filter + Offset) := False;
         when Single_Scale =>
            This.FS1R.FSC.Arr(Filter + Offset) := True;
      end case;
      case Fifo is
         when RX_Fifo_0 =>
            This.FFA1R.FFA.Arr(Filter + Offset) := False;
         when RX_Fifo_1 =>
            This.FFA1R.FFA.Arr(Filter + Offset) := True;
      end case;

      Enable_Filter (This, Filter);
   end Configure_Filter;

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               F1     : in Long_Filter;
                               F2     : in Long_Filter) is
   begin
      Configure_Filter (This, Filter, Fifo, Mode, Single_Scale,
                        To_Uint32(F1), To_Uint32(F2));
   end Configure_Filter;

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               F1     : in Short_Filter;
                               F2     : in Short_Filter;
                               F3     : in Short_Filter;
                               F4     : in Short_Filter) is
      W1 : UInt32;
      W2 : UInt32;
   begin
      W1 := Shift_Left(To_Uint32(F1), 16) and To_Uint32(F2);
      W2 := Shift_Left(To_Uint32(F3), 16) and To_Uint32(F4);

      Configure_Filter (This, Filter, Fifo, Mode, Dual_Scale, W1, W2);
   end Configure_Filter;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out CAN_Controller;
                   Msg : in Message; Mailbox : in TX_Mailbox) is
   begin
      case Mailbox is
         when TX_Mailbox_0 =>
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
            -- FIXME Time triggered communications?
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
         when TX_Mailbox_1 =>
            This.TDT1R.DLC := Msg.Length;
            This.TI1R.STID := Msg.Standard_Id;
            if Msg.Extended then
               This.TI1R.IDE := True; 
               This.TI1R.EXID := Msg.Extended_Id;
            else
               This.TI1R.IDE := False; 
            end if;
            if Msg.Remote then
               This.TI1R.RTR := True;
            else
               This.TI1R.RTR := False;
            end if;
            -- FIXME Time triggered communications?
            -- This.TDT1R.TGT := xxx;
            -- This.TDT1R.TIME := xxx;
            This.TDL1R.Arr(0) := Msg.Data(0);
            This.TDL1R.Arr(1) := Msg.Data(1);
            This.TDL1R.Arr(2) := Msg.Data(2);
            This.TDL1R.Arr(3) := Msg.Data(3);
            This.TDH1R.Arr(4) := Msg.Data(4);
            This.TDH1R.Arr(5) := Msg.Data(5);
            This.TDH1R.Arr(6) := Msg.Data(6);
            This.TDH1R.Arr(7) := Msg.Data(7);
            This.TI1R.TXRQ := True;
         when TX_Mailbox_2 =>
            This.TDT2R.DLC := Msg.Length;
            This.TI2R.STID := Msg.Standard_Id;
            if Msg.Extended then
               This.TI2R.IDE := True; 
               This.TI2R.EXID := Msg.Extended_Id;
            else
               This.TI2R.IDE := False; 
            end if;
            if Msg.Remote then
               This.TI2R.RTR := True;
            else
               This.TI2R.RTR := False;
            end if;
            -- FIXME Time triggered communications?
            -- This.TDT2R.TGT := xxx;
            -- This.TDT2R.TIME := xxx;
            This.TDL2R.Arr(0) := Msg.Data(0);
            This.TDL2R.Arr(1) := Msg.Data(1);
            This.TDL2R.Arr(2) := Msg.Data(2);
            This.TDL2R.Arr(3) := Msg.Data(3);
            This.TDH2R.Arr(4) := Msg.Data(4);
            This.TDH2R.Arr(5) := Msg.Data(5);
            This.TDH2R.Arr(6) := Msg.Data(6);
            This.TDH2R.Arr(7) := Msg.Data(7);
            This.TI2R.TXRQ := True;
      end case;
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive (This : in out CAN_Controller; Msg : out Message; Fifo : in RX_Fifo) is
   begin
      case Fifo is
         when RX_Fifo_0 =>
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
         when RX_Fifo_1 =>
            Msg.Length := This.RDT1R.DLC;
            if This.RI1R.IDE then
               Msg.Extended := True;
               Msg.Extended_Id := This.RI1R.EXID;
            else
               Msg.Extended := False;
            end if;
            Msg.Standard_Id := This.RI1R.STID;
            Msg.Remote := This.RI1R.RTR;
            Msg.Data(0) := This.RDL1R.Arr(0);
            Msg.Data(1) := This.RDL1R.Arr(1);
            Msg.Data(2) := This.RDL1R.Arr(2);
            Msg.Data(3) := This.RDL1R.Arr(3);
            Msg.Data(4) := This.RDH1R.Arr(4);
            Msg.Data(5) := This.RDH1R.Arr(5);
            Msg.Data(6) := This.RDH1R.Arr(6);
            Msg.Data(7) := This.RDH1R.Arr(7);
            This.RF1R.RFOM1 := True;
      end case;
   end Receive;

   -----------------------
   -- Message_Available --
   -----------------------
                                                                                
   function Message_Available (This : CAN_Controller) return Boolean is
      (This.RF0R.FMP0 /= 0 or This.RF1R.FMP1 /= 0);

   function Message_Available (This : CAN_Controller; Fifo : RX_Fifo) return Boolean is
      (if Fifo = Rx_Fifo_0 then This.RF0R.FMP0 /= 0 else This.RF1R.FMP1 /= 0);

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
      This.MCR.INRQ := True;
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
      This.MCR.INRQ := False;
      -- FIXME There should be timeout here, and some form of error reporting
      -- in case the CAN bus is not ok
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

   procedure Enable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt) is
   begin
      case Source is
         when Transmit_Mailbox_Empty =>
            This.IER.TMEIE := True;
         when Fifo0_Message_Pending =>
            This.IER.FMPIE0 := True;
         when Fifo0_Full =>
            This.IER.FFIE0 := True;
         when Fifo0_Overrun =>
            This.IER.FOVIE0 := True;
         when Fifo1_Message_Pending =>
            This.IER.FMPIE1 := True;
         when Fifo1_Full =>
            This.IER.FFIE1 := True;
         when Fifo1_Overrun =>
            This.IER.FOVIE1 := True;
         when Error_Warning =>
            This.IER.EWGIE := True;
         when Error_Passive =>
            This.IER.EPVIE := True;
         when Bus_Off =>
            This.IER.BOFIE := True;
         when Last_Error_Code =>
            This.IER.LECIE := True;
         when Error =>
            This.IER.ERRIE := True;
         when Wakeup =>
            This.IER.WKUIE := True;
         when Sleep =>
            This.IER.SLKIE := True;
      end case;
   end Enable_Interrupts;

   procedure Disable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt) is
   begin
      case Source is
         when Transmit_Mailbox_Empty =>
            This.IER.TMEIE := False;
         when Fifo0_Message_Pending =>
            This.IER.FMPIE0 := False;
         when Fifo0_Full =>
            This.IER.FFIE0 := False;
         when Fifo0_Overrun =>
            This.IER.FOVIE0 := False;
         when Fifo1_Message_Pending =>
            This.IER.FMPIE1 := False;
         when Fifo1_Full =>
            This.IER.FFIE1 := False;
         when Fifo1_Overrun =>
            This.IER.FOVIE1 := False;
         when Error_Warning =>
            This.IER.EWGIE := False;
         when Error_Passive =>
            This.IER.EPVIE := False;
         when Bus_Off =>
            This.IER.BOFIE := False;
         when Last_Error_Code =>
            This.IER.LECIE := False;
         when Error =>
            This.IER.ERRIE := False;
         when Wakeup =>
            This.IER.WKUIE := False;
         when Sleep =>
            This.IER.SLKIE := False;
      end case;
   end Disable_Interrupts;

   function Interrupt_Enabled
     (This   : CAN_Controller;
      Source : CAN_Interrupt)
      return Boolean is
   begin
      case Source is
         when Transmit_Mailbox_Empty =>
            return This.IER.TMEIE;
         when Fifo0_Message_Pending =>
            return This.IER.FMPIE0;
         when Fifo0_Full =>
            return This.IER.FFIE0;
         when Fifo0_Overrun =>
            return This.IER.FOVIE0;
         when Fifo1_Message_Pending =>
            return This.IER.FMPIE1;
         when Fifo1_Full =>
            return This.IER.FFIE1;
         when Fifo1_Overrun =>
            return This.IER.FOVIE1;
         when Error_Warning =>
            return This.IER.EWGIE;
         when Error_Passive =>
            return This.IER.EPVIE;
         when Bus_Off =>
            return This.IER.BOFIE;
         when Last_Error_Code =>
            return This.IER.LECIE;
         when Error =>
            return This.IER.ERRIE;
         when Wakeup =>
            return This.IER.WKUIE;
         when Sleep =>
            return This.IER.SLKIE;
      end case;
   end Interrupt_Enabled;

   function To_UInt3 is new Ada.Unchecked_Conversion(Error_Code, UInt3);

   function To_Error_Code is new Ada.Unchecked_Conversion(UInt3, Error_Code);

   function Status (This : CAN_Controller; Flag : CAN_Status_Flag)
     return Boolean is
   begin
      case Flag is
         when Transmit_Mailbox_Empty_Indicated =>
            return (This.TSR.TME.Arr(0) or This.TSR.TME.Arr(1) or This.TSR.TME.Arr(2));
         when Fifo0_Message_Pending_Indicated =>
            return (This.RF0R.FMP0 > 0);
         when Fifo0_Full_Indicated =>
            return This.RF0R.FULL0;
         when Fifo0_Overrun_Indicated =>
            return This.RF0R.FOVR0;
         when Fifo1_Message_Pending_Indicated =>
            return (This.RF1R.FMP1 > 0);
         when Fifo1_Full_Indicated =>
            return This.RF1R.FULL1;
         when Fifo1_Overrun_Indicated =>
            return This.RF1R.FOVR1;
         when Error_Warning_Indicated =>
            return This.ESR.EWGF;
         when Error_Passive_Indicated =>
            return This.ESR.EPVF;
         when Bus_Off_Indicated =>
            return This.ESR.BOFF;
         when Last_Error_Code_Indicated =>
            return (This.ESR.LEC /= To_UInt3(No_Error) and This.ESR.LEC /= To_UInt3(Set_By_Software));
         when Error_Indicated =>
            return (This.MSR.ERRI);
         when Wakeup_Indicated =>
            return (This.MSR.WKUI);
         when Sleep_Indicated =>
            return (This.MSR.SLAKI);
      end case;
   end Status;

   procedure Clear_Status (This : in out CAN_Controller; Flag : CAN_Status_Flag) is
   begin
      case Flag is
         when Last_Error_Code_Indicated =>
            This.ESR.LEC := To_Uint3(Set_By_Software);
         when Error_Indicated =>
            This.MSR.ERRI := False;
         when Wakeup_Indicated =>
            This.MSR.WKUI := False;
         when Sleep_Indicated =>
            This.MSR.SLAKI := False;
         when others =>
            null;
      end case;
   end Clear_Status;

   function Last_Error_Code (This : CAN_Controller)
     return Error_Code is (To_Error_Code(This.ESR.LEC));

   function Receive_Error_Counter (This : CAN_Controller)
     return UInt8 is (This.ESR.REC);

   function Transmit_Error_Counter (This : CAN_Controller)
     return UInt8 is (This.ESR.TEC);

end STM32.CAN;
