{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Api.ProtocolParameters (ProtocolParameters) where

import Cardano.Api.Shelley
import Data.Default (Default, def)
import Data.Map (fromList)
import Data.Ratio ((%))

instance Default ProtocolParameters where
    def = ProtocolParameters
        { protocolParamProtocolVersion = (5,0)
        , protocolParamDecentralization = 3 % 5
        , protocolParamExtraPraosEntropy = Nothing
        , protocolParamMaxBlockHeaderSize = 1100
        , protocolParamMaxBlockBodySize = 65536
        , protocolParamMaxTxSize = 16384
        , protocolParamTxFeeFixed = 155381
        , protocolParamTxFeePerByte = 44
        , protocolParamMinUTxOValue = Nothing
        , protocolParamStakeAddressDeposit = Lovelace 2000000
        , protocolParamStakePoolDeposit = Lovelace 500000000
        , protocolParamMinPoolCost = Lovelace 340000000
        , protocolParamPoolRetireMaxEpoch = EpochNo 18
        , protocolParamStakePoolTargetNum = 150
        , protocolParamPoolPledgeInfluence = 3 % 10
        , protocolParamMonetaryExpansion = 3 % 1000
        , protocolParamTreasuryCut = 1 % 5
        , protocolParamUTxOCostPerWord = Just (Lovelace 1)
        , protocolParamCostModels = fromList
            [ (AnyPlutusScriptVersion PlutusScriptV1, CostModel (fromList
                [("add_integer-cpu-arguments-intercept",235735)
                ,("add_integer-cpu-arguments-slope",1722)
                ,("add_integer-memory-arguments-intercept",1)
                ,("add_integer-memory-arguments-slope",1)
                ,("cek_apply_cost-_ex_budget_cpu",39000)
                ,("cek_apply_cost-_ex_budget_memory",10)
                ,("cek_builtin_cost-_ex_budget_cpu",39000)
                ,("cek_builtin_cost-_ex_budget_memory",10)
                ,("cek_const_cost-_ex_budget_cpu",39000)
                ,("cek_const_cost-_ex_budget_memory",10)
                ,("cek_delay_cost-_ex_budget_cpu",39000)
                ,("cek_delay_cost-_ex_budget_memory",10)
                ,("cek_force_cost-_ex_budget_cpu",39000)
                ,("cek_force_cost-_ex_budget_memory",10)
                ,("cek_lam_cost-_ex_budget_cpu",39000)
                ,("cek_lam_cost-_ex_budget_memory",10)
                ,("cek_startup_cost-_ex_budget_cpu",1000000)
                ,("cek_startup_cost-_ex_budget_memory",0)
                ,("cek_var_cost-_ex_budget_cpu",39000)
                ,("cek_var_cost-_ex_budget_memory",10)
                ,("concatenate-cpu-arguments-intercept",420084)
                ,("concatenate-cpu-arguments-slope",515)
                ,("concatenate-memory-arguments-intercept",0)
                ,("concatenate-memory-arguments-slope",1)
                ,("divide_integer-cpu-arguments-model_split_const_intercept",330895)
                ,("divide_integer-cpu-arguments-model_split_const_slope",427)
                ,("divide_integer-memory-arguments-intercept",0)
                ,("divide_integer-memory-arguments-minimum",1)
                ,("divide_integer-memory-arguments-slope",1)
                ,("drop_byte_string-cpu-arguments",3418326)
                ,("drop_byte_string-memory-arguments",2)
                ,("eq_byte_string-cpu-arguments-intercept",188562)
                ,("eq_byte_string-cpu-arguments-slope",246)
                ,("eq_byte_string-memory-arguments",1)
                ,("eq_integer-cpu-arguments-intercept",211716)
                ,("eq_integer-cpu-arguments-slope",867)
                ,("eq_integer-memory-arguments",1)
                ,("greater_than_eq_integer-cpu-arguments-intercept",216015)
                ,("greater_than_eq_integer-cpu-arguments-slope",699)
                ,("greater_than_eq_integer-memory-arguments",1)
                ,("greater_than_integer-cpu-arguments-intercept",187278)
                ,("greater_than_integer-cpu-arguments-slope",980)
                ,("greater_than_integer-memory-arguments",1)
                ,("gt_byte_string-cpu-arguments-intercept",204537)
                ,("gt_byte_string-cpu-arguments-slope",227)
                ,("gt_byte_string-memory-arguments",1)
                ,("if_then_else-cpu-arguments",0)
                ,("if_then_else-memory-arguments",0)
                ,("less_than_eq_integer-cpu-arguments-intercept",216015)
                ,("less_than_eq_integer-cpu-arguments-slope",699)
                ,("less_than_eq_integer-memory-arguments",1)
                ,("less_than_integer-cpu-arguments-intercept",187278)
                ,("less_than_integer-cpu-arguments-slope",980)
                ,("less_than_integer-memory-arguments",1)
                ,("lt_byte_string-cpu-arguments-intercept",204537)
                ,("lt_byte_string-cpu-arguments-slope",227)
                ,("lt_byte_string-memory-arguments",1)
                ,("mod_integer-cpu-arguments-model_split_const_intercept",330895)
                ,("mod_integer-cpu-arguments-model_split_const_slope",427)
                ,("mod_integer-memory-arguments-intercept",0)
                ,("mod_integer-memory-arguments-minimum",1)
                ,("mod_integer-memory-arguments-slope",1)
                ,("multiply_integer-cpu-arguments-intercept",78642)
                ,("multiply_integer-cpu-arguments-slope",11464)
                ,("multiply_integer-memory-arguments-intercept",0)
                ,("multiply_integer-memory-arguments-slope",1)
                ,("quotient_integer-cpu-arguments-model_split_const_intercept",330895)
                ,("quotient_integer-cpu-arguments-model_split_const_slope",427)
                ,("quotient_integer-memory-arguments-intercept",0)
                ,("quotient_integer-memory-arguments-slope",1)
                ,("remainder_integer-cpu-arguments-model_split_const_intercept",330895)
                ,("remainder_integer-cpu-arguments-model_split_const_slope",427)
                ,("remainder_integer-memory-arguments-intercept",0)
                ,("remainder_integer-memory-arguments-slope",1)
                ,("sha2-cpu-arguments-intercept",2267819)
                ,("sha2-cpu-arguments-slope",28904)
                ,("sha2-memory-arguments",4)
                ,("sha3-cpu-arguments-intercept",1260296)
                ,("sha3-cpu-arguments-slope",81356)
                ,("sha3-memory-arguments",4)
                ,("subtract_integer-cpu-arguments-intercept",251934)
                ,("subtract_integer-cpu-arguments-slope",1194)
                ,("subtract_integer-memory-arguments-intercept",1)
                ,("subtract_integer-memory-arguments-slope",1)
                ,("take_byte_string-cpu-arguments",3420288)
                ,("take_byte_string-memory-arguments",20)
                ,("verify_signature-cpu-arguments",5082989)
                ,("verify_signature-memory-arguments",1)
                ]))
            ]
        , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 1 % 1, priceExecutionMemory = 1 % 1})
        , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 500000000000, executionMemory = 500000000000})
        , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 500000000000, executionMemory = 500000000000})
        , protocolParamMaxValueSize = Just 5000
        , protocolParamCollateralPercent = Just 1
        , protocolParamMaxCollateralInputs = Just 5
        }
