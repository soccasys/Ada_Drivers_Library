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

--  This file provides interfaces for the CAN controllers on the
--  STM32F4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

with System;
private with STM32_SVD.CAN;

package STM32.CAN is

   type CAN_Controller is limited private;

   type Retransmission_Mode is (Automatic, Non_Automatic);

   type Test_Mode is (Silent, Loopback, Silent_Loopback);

   type Bit_Rate is
     (Rate_50KBPS,
      Rate_100KBPS,
      Rate_125KBPS,
      Rate_250KBPS,
      Rate_500KBPS,
      Rate_1000KBPS);

   type Payload is array (0 .. 7) of Uint8;

   type Message is record
      Standard_Id : Uint11;
      Extended_Id : Uint18;
      Extended    : Boolean;
      Remote      : Boolean;
      Data        : Payload;
      Length      : Uint4;
   end record;

   type Filter_Mode is (Mask_Mode, List_Mode);

   type Filter_Scale is (Single_Scale, Dual_Scale);

   type RX_Fifo is (RX_Fifo_0, RX_Fifo_1);

   type TX_Mailbox is (TX_Mailbox_0, TX_Mailbox_1, TX_Mailbox_2);

   type Long_Filter is record
      --  unspecified
      Reserved_0 : Boolean := False;
      --  RTR
      RTR        : Boolean := False;
      --  IDE
      IDE        : Boolean := False;
      --  EXID
      EXID       : HAL.UInt18 := 16#0#;
      --  STID
      STID       : HAL.UInt11 := 16#0#;
   end record
     with Size => 32, Bit_Order => System.Low_Order_First; 

   for Long_Filter use record
      Reserved_0 at 0 range 0 .. 0;
      RTR        at 0 range 1 .. 1;
      IDE        at 0 range 2 .. 2;
      EXID       at 0 range 3 .. 20;
      STID       at 0 range 21 .. 31;
   end record;
   
   type Short_Filter is record
      --  EXID
      EXID       : HAL.UInt3 := 16#0#;
      --  RTR
      RTR        : Boolean := False;
      --  IDE
      IDE        : Boolean := False;
      --  STID
      STID       : HAL.UInt11 := 16#0#;
   end record
     with Size => 16, Bit_Order => System.Low_Order_First; 

   for Short_Filter use record
      EXID       at 0 range 0 .. 2;
      RTR        at 0 range 3 .. 3;
      IDE        at 0 range 4 .. 4;
      STID       at 0 range 5 .. 15;
   end record;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This : in out CAN_Controller; Rate : Bit_Rate; Mode : Retransmission_Mode);

   -------------------
   -- Enable_Filter --
   -------------------

   procedure Enable_Filter (This : in out CAN_Controller; Filter : in Integer);

   --------------------
   -- Disable_Filter --
   --------------------

   procedure Disable_Filter (This : in out CAN_Controller; Filter : in Integer);

   -------------------------
   -- Disable_All_Filters --
   -------------------------

   procedure Disable_All_Filters (This : in out CAN_Controller);

   ----------------------
   -- Configure_Filter --
   ----------------------

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               Scale  : in Filter_Scale;
                               W1     : in UInt32;
                               W2     : in UInt32);

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               F1     : in Long_Filter;
                               F2     : in Long_Filter) with Inline;

   procedure Configure_Filter (This   : in out CAN_Controller;
                               Filter : in Integer;
                               Fifo   : in RX_Fifo;
                               Mode   : in Filter_Mode;
                               F1     : in Short_Filter;
                               F2     : in Short_Filter;
                               F3     : in Short_Filter;
                               F4     : in Short_Filter) with Inline;

   ----------
   -- Send --
   ----------

   procedure Send (This : in out CAN_Controller; Msg : in Message; Mailbox : in TX_Mailbox);

   -------------
   -- Receive --
   -------------

   procedure Receive (This : in out CAN_Controller; Msg : out Message; Fifo : in RX_Fifo);

   -----------------------
   -- Message_Available --
   -----------------------

   function Message_Available (This : CAN_Controller) return Boolean;

   function Message_Available (This : CAN_Controller; Fifo : RX_Fifo) return Boolean;

   ----------------------------
   -- In_Initialization_Mode --
   ----------------------------

   function In_Initialization_Mode (This : CAN_Controller) return Boolean;

   --------------------
   -- In_Normal_Mode --
   --------------------

   function In_Normal_Mode (This : CAN_Controller) return Boolean;

   -------------------
   -- In_Sleep_Mode --
   -------------------

   function In_Sleep_Mode (This : CAN_Controller) return Boolean;

   -------------------------------
   -- Enter_Initialization_Mode --
   -------------------------------

   procedure Enter_Initialization_Mode (This : in out CAN_Controller);

   -----------------------
   -- Enter_Normal_Mode --
   -----------------------

   procedure Enter_Normal_Mode (This : in out CAN_Controller);

   ----------------------
   -- Enter_Sleep_Mode --
   ----------------------

   procedure Enter_Sleep_Mode (This : in out CAN_Controller);

   ----------------------
   -- Enable_Test_Mode --
   ----------------------

   procedure Enable_Test_Mode (This : in out CAN_Controller; Mode : Test_Mode);

   -----------------------
   -- Disable_Test_Mode --
   -----------------------

   procedure Disable_Test_Mode (This : in out CAN_Controller; Mode : Test_Mode);

   ---------------------------------
   -- Enable_Filter_Configuration --
   ---------------------------------

   procedure Enable_Filter_Configuration (This : in out CAN_Controller);

   ----------------------------------
   -- Disable_Filter_Configuration --
   ----------------------------------

   procedure Disable_Filter_Configuration (This : in out CAN_Controller);

   type CAN_Interrupt is
     (Transmit_Mailbox_Empty,
      Fifo0_Message_Pending,
      Fifo0_Full,
      Fifo0_Overrun,
      Fifo1_Message_Pending,
      Fifo1_Full,
      Fifo1_Overrun,
      Error_Warning,
      Error_Passive,
      Bus_Off,
      Last_Error_Code,
      Error,
      Wakeup,
      Sleep);

   -----------------------
   -- Enable_Interrupts --
   -----------------------

   procedure Enable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt)
     with
       Post => Interrupt_Enabled (This, Source),
       Inline;

   ------------------------
   -- Disable_Interrupts --
   ------------------------

   procedure Disable_Interrupts
     (This   : in out CAN_Controller;
      Source : CAN_Interrupt)
     with
       Post => not Interrupt_Enabled (This, Source),
       Inline;

   -----------------------
   -- Interrupt_Enabled --
   -----------------------

   function Interrupt_Enabled
     (This   : CAN_Controller;
      Source : CAN_Interrupt)
      return Boolean
     with Inline;

   type CAN_Status_Flag is
     (Transmit_Mailbox_Empty_Indicated,
      Fifo0_Message_Pending_Indicated,
      Fifo0_Full_Indicated,
      Fifo0_Overrun_Indicated,
      Fifo1_Message_Pending_Indicated,
      Fifo1_Full_Indicated,
      Fifo1_Overrun_Indicated,
      Error_Warning_Indicated,
      Error_Passive_Indicated,
      Bus_Off_Indicated,
      Last_Error_Code_Indicated,
      Error_Indicated,
      Wakeup_Indicated,
      Sleep_Indicated);

   function Status (This : CAN_Controller; Flag : CAN_Status_Flag)
     return Boolean with Inline;

   procedure Clear_Status (This : in out CAN_Controller; Flag : CAN_Status_Flag)
     with Inline;

   type Error_Code is
     (No_Error,
      Stuff_Error,
      Form_Error,
      Acknowledgement_Error,
      Bit_Recessive_Error,
      Bit_Dominant_Error,
      CRC_Error,
      Set_By_Software);

   for Error_Code use
     (No_Error              => 2#000#,
      Stuff_Error           => 2#001#,
      Form_Error            => 2#010#,
      Acknowledgement_Error => 2#011#,
      Bit_Recessive_Error   => 2#100#,
      Bit_Dominant_Error    => 2#101#,
      CRC_Error             => 2#110#,
      Set_By_Software       => 2#111#);

   function Last_Error_Code (This : CAN_Controller)
     return Error_Code with Inline;

   function Receive_Error_Counter (This : CAN_Controller)
     return UInt8 with Inline;

   function Transmit_Error_Counter (This : CAN_Controller)
     return UInt8 with Inline;

private

   type CAN_Controller is new STM32_SVD.CAN.CAN_Peripheral;

   -- Hack to access filter configuration as an array
   type CAN_Filters is array ( 0 .. 55 ) of UInt32;

   CAN12_Filters : aliased CAN_Filters with Import, Volatile, Address => STM32_SVD.CAN.CAN1_Periph.F0R1'Address;

end STM32.CAN;
