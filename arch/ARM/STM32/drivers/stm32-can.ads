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
--   @file    stm32f4xx_hal_dac.h and stm32f4xx_hal_dac_ex.h                --
--   @author  MCD Application Team                                          --
--   @version V1.3.1                                                        --
--   @date    25-March-2015                                                 --
--   @brief   Header file of DAC HAL module.                                --
--                                                                          --
--   COPYRIGHT(c) 2014 STMicroelectronics                                   --
------------------------------------------------------------------------------

--  This file provides interfaces for the digital-to-analog converters on the
--  STM32F4 (ARM Cortex M4F) microcontrollers from ST Microelectronics.

private with STM32_SVD.CAN;

package STM32.CAN is

   type CAN_Controller is limited private;

   type Retransmission_Mode is (Automatic, Non_Automatic);

   type Test_Mode is (Silent, Loopback, Silent_Loopback);

   type Bit_Rate is (Rate_50KBPS, Rate_100KBPS, Rate_125KBPS, Rate_250KBPS, Rate_500KBPS, Rate_1000KBPS);

   type Payload is array (0 .. 7) of Uint8;

   type Message is record
      Standard_Id : Uint11;
      Extended_Id : Uint18;
      Extended    : Boolean;
      Remote      : Boolean;
      Data        : Payload;
      Length      : Uint4;
   end record;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (This : in out CAN_Controller; Rate : Bit_Rate; Mode : Retransmission_Mode);

   ----------------------
   -- Configure_Filter --
   ----------------------

   procedure Configure_Filter (This : CAN_Controller);

   ----------
   -- Send --
   ----------

   procedure Send (This : in out CAN_Controller; Msg : in Message);

   -------------
   -- Receive --
   -------------

   procedure Receive (This : in out CAN_Controller; Msg : out Message);

   -----------------------
   -- Message_Available --
   -----------------------

   function Message_Available (This : CAN_Controller) return Boolean;

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

private

   type CAN_Controller is new STM32_SVD.CAN.CAN_Peripheral;

end STM32.CAN;
