USE [ARTS_DWM_3_2014-01-16]
GO

--------------------------------------------------------------------------------
-- Build a stored summary table to use for the SALES and ITEM related sample  --
-- customer performance measure queries.  This is an intermediate rowset      --
-- that is used as input to the queries used by differen performance measures --
--------------------------------------------------------------------------------
-- Create VW_DW3_CT_SLSRTN_STRD_SMRY - Customer Sale/Return Stored Summary    --
-- View                                                                       --
--------------------------------------------------------------------------------
--drop view VW_DW3_CT_SLSRTN_STRD_SMRY;
create view VW_DW3_CT_SLSRTN_STRD_SMRY as
with SLSRTN as
    (
    ----------------------------------------------------------------------------
    -- This subquery assembles and calculates LINE ITEM LEVEL net values to   --
    -- make the summarization mainline query simpler.  This also helps in     --
    -- debugging.                                                             --
    ----------------------------------------------------------------------------
         select
             ID_CT                                         -- Customer ID
            ,DC_DY_BSN                                     -- Business Day Date
            ,ID_BSN_UN                                     -- Business Unit (store)
            ,ID_CHNL                                       -- Channel
            ,ID_TRN                                        -- Transaction
            ,IC_LN_ITM                                     -- Transaction Line Item
            ,MO_EXTND                                      -- Extended actual amount
            --------------------------------------------------------------------
            -- Logic here handles bulk items and items sold in retail selling --
            -- units.  They have different ways handling quantity sold.  The  --
            -- retail selling unit type items have to handle a "2 for" or     --
            -- "3 for" unit price that applies to sets of retail selling      --
            -- units.  This carries over to the calculation of net margin     --
            --------------------------------------------------------------------
            ,QU_UN 
            ,QU_ITM_LM_RTN_SLS
                                                             
            ,case
                when TY_ITM_STK = 'BULK'  and QU_UN > 0
                    then CP_INV * QU_UN                       -- Bulk item extended cost
                else
                    (CP_INV/UN_INV_UPRQY) * QU_ITM_LM_RTN_SLS  -- Retail Selling Unit ext cost
             end as MO_CST_INV_EXTND                           -- Inventory Cost
            ,case
                when TY_ITM_STK = 'BULK' and QU_UN > 0
                    then MO_EXTND - (CP_INV * QU_UN)
                else 
                    MO_EXTND - ((CP_INV/UN_INV_UPRQY) * QU_ITM_LM_RTN_SLS)
             end as NET_MRGN                                   -- Line item net margin
             
         from
             DW3_FACT_SALE_RTN_BEHAVIOR
             join DW3_DIM_ITM
             on DW3_FACT_SALE_RTN_BEHAVIOR.ID_ITM = DW3_DIM_ITM.ID_ITM
         -----------------------------------------------------------------------
         -- For analytic purposes we're filtering out voided, canceled and    --
         -- training transactions.  If this query were being used for audit   --
         -- purposes, these should be included.                               --
         ----------------------------------------------------------------------- 
         where
              FL_CNCL = 0          -- Include only transactions that are NOT canceled
              AND FL_VD = 0        -- Include only transactioins that are NOT voided
              AND FL_SPN = 0       -- Include only transactions that are NOT suspended
              AND FL_TRG_TRN = 0   -- Include only transactions that are NOT training 
                                   -- transaction   
    )
--------------------------------------------------------------------------------
-- Summarize line items to CUSTOMER TRANSACTION level                         --
--------------------------------------------------------------------------------
select
     SLSRTN.ID_CT                                         -- Customer
    ,SLSRTN.DC_DY_BSN                                     -- Business Day Date
    ,SLSRTN.ID_BSN_UN                                     -- Business Unit (store)
    ,SLSRTN.ID_CHNL                                       -- Channel
    ,SLSRTN.ID_TRN                                        -- Transaction
    ,COUNT(SLSRTN.IC_LN_ITM) as QU_LN_ITM                 -- Transaction Line Item Count
    ,SUM(SLSRTN.MO_EXTND)    as MO_NT_SLS                 -- Transaction Net Sales
    ,SUM(SLSRTN.QU_ITM_LM_RTN_SLS) as QU_ITM_SLD          -- Quantity of items sold
                                                          -- (retail selling units)
    ,SUM(SLSRTN.QU_UN) as QU_BLK_ITM_SLD                  -- BULK item qty sold
    ,SUM(SLSRTN.MO_CST_INV_EXTND) as MO_COGS              -- Total INVENTORY COGS
    ,SUM(SLSRTN.NET_MRGN) as MO_NET_MRGN                  -- Net Margin
from
    SLSRTN 
group by   
     SLSRTN.ID_CT                                         -- Customer
    ,SLSRTN.DC_DY_BSN                                     -- Business Day Date
    ,SLSRTN.ID_BSN_UN                                     -- Business Unit (store)
    ,SLSRTN.ID_CHNL                                       -- Channel
    ,SLSRTN.ID_TRN                                        -- Transaction
;
--------------------------------------------------------------------------------
-- End VW_DW3_CT_SLSRTN_STRD_SMRY - Customer Sale/Return Stored Summary View  --
--------------------------------------------------------------------------------
select * INTO DW3_STRD_SMRY_CT_SLSRTN       -- Stored summary table for sample sales
from VW_DW3_CT_SLSRTN_STRD_SMRY             -- return performance measures
;
--------------------------------------------------------------------------------
-- END of Stored Summary Table for sample sales/returns performance measures  --
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Build a stored summary table to use for the TENDER related sample customer --
-- performance measures                                                       --
--------------------------------------------------------------------------------
-- Create View VW_DW3_CT_TNDR_STRD_SRMY - Customer Tender Stored Summary      --
-- View                                                                       --
--------------------------------------------------------------------------------
--drop view VW_DW3_CT_TNDR_STRD_SMRY;
create view VW_DW3_CT_TNDR_STRD_SMRY as
with TNDR as
    (
        select
             ID_CT
            ,DC_DY_BSN
            ,ID_BSN_UN
            ,ID_CHNL
            ,ID_TRN
            ,TY_TND
            ,MO_TRN_AMT
            --------------------------------------------------------------------
            -- Case statements set count indicators for all of the different  --
            -- kinds of tenders so we can analyze customer tender usage       --
            -- within and across transactions.                                --
            --------------------------------------------------------------------
            ,case
                 when TY_TND = 'CASH' then 1
                 else 0
             end as IND_CASH
            ,case
                 when TY_TND in ('CHEQUE','CHECK') then 1
                 else 0
             end as IND_CHECK
            ,case
                 when TY_TND in ('CREDIT','DEBIT') then 1
                 else 0
             end as IND_CRDB
            ,case
                 when TY_TND = 'COUPON' then 1
                 else 0
             end as IND_COUPON
            ,case
                 when TY_TND = 'STORED_VALUE' then 1
                 else 0
             end as IND_STORED_VALUE
            ,case
                 when TY_TND = 'CUST_ACCT' then 1
                 else 0
             end as IND_CUST_ACCT
            ,case
                 when TY_TND = 'TRADEIN' then 1
                 else 0
             end as IND_TRADEIN
            ,case
                 when TY_TND = 'EBT' then 1
                 else 0
             end as IND_EBT
           ---------------------------------------------------------------------
           -- Case statements to standardize handling differet credit/debit   --
           -- card issues and co branding for subsequent analysis             --
           ---------------------------------------------------------------------
           ,case
               when TY_TND in ('CREDIT','DEBIT') then
                   CRDB_TY_CRD
               else 'NON_CRDB_TENDER'
            end as CRDB_TY_CRD
           ,case
               when TY_TND in ('CREDIT','DEBIT') then
                   CRDB_ID_TND_MD_BRN
               else 'NON_CRDB_TENDER'
            end as CRDB_ID_TND_MD_BRN
          ,CD_CNY_ISO_4217_LCL       -- Retailers local currency 
          ,CD_CNY_ISO_4217_TND       -- The currency tendered in this line item
          ,case
              when CD_CNY_ISO_4217_LCL <> CD_CNY_ISO_4217_TND then 1
              else 0
           end as CNY_CNV_IND        -- use to count currency conversions for analysis
           ---------------------------------------------------------------------
           -- Tender monetary values split out by tender type for rollup      --
           -- In this sample we are omitting the possible special handling    --
           -- fees charged by a retailer for converting foreign currency      --
           -- taken as payment and foreign currency returned as change to a   --
           -- customer.  We include only the local currency value, exchange   --
           -- rate and foreign currency value.                                --
           ---------------------------------------------------------------------
          ,CSH_MO_ITM_LN_TND  -- Amount of cash tendered by the customer
          ,CSH_MO_TRN_AMT     -- Amount of cash tender applied to settle trans
          ,CSH_MO_FRG_CY      -- Foreign currency amount tender applier
          ,CSH_MO_RTE_EXC     -- Cash exchange rate (NON-ADDITIVE fact)
          ----------------------------------------------------------------------
          -- Change returned to customer as part of settlement where tender   --
          -- value of payment exceeds the total sale value.                   --
          ----------------------------------------------------------------------
          ,CHG_MO_ITM_LN_TND  -- Change amount returned to customer (in retail local currency) 
          ,CHG_MO_FRG_CY      -- Change amount returned to customer in foreign currency
          ,CHG_MO_RTE_EXC     -- Change exchange rate
          ----------------------------------------------------------------------
          -- In this sample, we are assuming that checks will be denominated  --
          -- in the retailer's local currency                                 --
          ----------------------------------------------------------------------
          ,CHK_MO_ITM_LN_TND  -- Check amount tendered by the customer
          ,CHK_MO_TRN_AMT     -- Amount of check tender applied to settle transaction
          ----------------------------------------------------------------------
          ,CRDB_MO_ITM_LN_TND -- Credit Debit card amount tendered
          ,CRDB_MO_TRN_AMT    -- Credit Debit card amount applied to transaction
          ----------------------------------------------------------------------
          ,CPN_MO_ITM_LN_TND  -- Coupon amount tendered
          ,CPN_MO_TRN_AMT     -- Coupon amount applied to transaction
          ----------------------------------------------------------------------
          ,GF_CF_MO_ITM_LN_TND -- Gift Cert (stored value) amount tendered
          ,GF_CF_MO_TRN_AMT    -- Gift Cert amount applied to transaction
          ----------------------------------------------------------------------
          ,CTAC_MO_ITM_LN_TND  -- Total Customer Account charged (A/R debit)
          ,CTAC_MO_TRN_AMT     -- Customer Account charge applied to transaction
          ----------------------------------------------------------------------
          ,TRADEIN_MO_ITM_LN_TND -- Tradein Total amount tendered
          ,TRADEIN_MO_TRN_AMT    -- Tradein amount applied to transaction
          ----------------------------------------------------------------------
          ,EBT_MO_ITM_LN_TND     -- EBT total amount tendered
          ,EBT_MO_TRN_AMT        -- EBT amount applied to transaction
          ----------------------------------------------------------------------

        from
            DW3_FACT_TENDER_BEHAVIOR
        where
              FL_CNCL = 0          -- Include only transactions that are NOT canceled
              AND FL_VD = 0        -- Include only transactioins that are NOT voided
              AND FL_SPN = 0       -- Include only transactions that are NOT suspended
              AND FL_TRG_TRN = 0   -- Include only transactions that are NOT training 
                                   -- transaction   
    )
--------------------------------------------------------------------------------
-- Note we will aggregate ALL tender line items into a single row that counts --
-- line items by tender type and returns the tender value actuall assigned to --
-- the transaction.  This is aimed at supporting the spreadsheet tender       --
-- related performance measures.                                              --
--------------------------------------------------------------------------------
select
     TNDR.ID_CT
    ,TNDR.DC_DY_BSN
    ,TNDR.ID_BSN_UN
    ,TNDR.ID_CHNL
    ,TNDR.ID_TRN
    ,TNDR.TY_TND
    ----------------------------------------------------------------------------
    -- In this sample ALL tender amounts are in the retailer's local currency --
    -- since the performance measures were silent on foreign currncy.         --
    ----------------------------------------------------------------------------
    ,SUM(TNDR.MO_TRN_AMT) AS MO_TRN_ALL_TNDR_APPLD
    ,SUM(TNDR.IND_CASH) AS CASH_TNDR_LN_ITM_COUNT
    ,SUM(TNDR.IND_CHECK) AS CHECK_TNDR_LN_ITM_COUNT
    ,SUM(TNDR.IND_CRDB) AS CRDB_TNDR_LN_ITM_COUNT
    ,SUM(TNDR.IND_COUPON) AS CPN_TNDR_LN_ITM_COUNT
    ,SUM(TNDR.IND_STORED_VALUE) AS STRD_VL_LN_ITM_COUNT
    ,SUM(TNDR.IND_CUST_ACCT) AS CT_ACT_LN_ITM_COUNT
    ,SUM(TNDR.IND_TRADEIN) AS TRADEIN_LN_ITM_COUNT
    ,SUM(TNDR.IND_EBT) AS EBT_LN_ITM_COUNT
    ,SUM(TNDR.CSH_MO_TRN_AMT) AS MO_TRN_TOT_APPLD
    ,SUM(TNDR.CHK_MO_TRN_AMT) AS MO_CHK_TRN_TOT_APPLD
    ,SUM(TNDR.CRDB_MO_TRN_AMT) AS MO_DBCR_TRN_TOT_APPLD
    ,SUM(TNDR.CPN_MO_TRN_AMT) AS MO_CPN_TRN_TOT_APPLD
    ,SUM(TNDR.GF_CF_MO_TRN_AMT) AS MO_GF_CF_TRN_TOT_APPLD
    ,SUM(TNDR.CTAC_MO_TRN_AMT) AS MO_CTAC_TRN_TOT_APPLD
    ,SUM(TNDR.TRADEIN_MO_TRN_AMT) AS MO_TRADEIN_TRN_TOT_APPLD
    ,SUM(TNDR.EBT_MO_TRN_AMT) AS MO_EBT_TRN_TOT_APPLD
from
     TNDR    
group by
     TNDR.ID_CT
    ,TNDR.DC_DY_BSN
    ,TNDR.ID_BSN_UN
    ,TNDR.ID_CHNL
    ,TNDR.ID_TRN
    ,TNDR.TY_TND
;
--------------------------------------------------------------------------------
-- END View VW_DW3_CT_TNDR_ST_SRMY - Customer Tender Stored Summary Table     --
--------------------------------------------------------------------------------
DROP TABLE DW3_STRD_SMRY_CT_TNDR;

select * INTO DW3_STRD_SMRY_CT_TNDR       -- Stored summary table for sample sales
from VW_DW3_CT_TNDR_STRD_SMRY               -- return performance measures
;
--------------------------------------------------------------------------------
-- END of Stored Summary Table for sample tender performance measures         --
--------------------------------------------------------------------------------


---------------------------------------------------------------------------------
-- Create View VW_RP_MNTH_COUNT - This view provides a month calendar period   --
-- count for each reporting period.  This is used when an average monthly      --
-- value is to be calculated to a REPORTING PERIOD.                            --
-- NOTE: This pattern can be used to create other calendar time period counts  --
-- for reporting periods by changing the 'PERIOD' to a different value         --
---------------------------------------------------------------------------------
--drop view VW_RP_MNTH_COUNT;
CREATE VIEW VW_RP_MNTH_COUNT as
WITH RP_PRD_CNT AS
    (
		select DISTINCT
			 NM_PRD_RP
			,NM_CLD
			,dbo.FN_RP_CP_EXTR2
				(
				   CLD_PRD_NM_PTH
				  ,CLD_PRD_HRC_LVL_NM_PTH
				  ,'|'
				  ,'PERIOD'
				) AS RP_MNTH
		FROM
			DW3_DIM_CA_PRD_RP
    )
SELECT
     NM_PRD_RP
	,NM_CLD
	,COUNT(RP_MNTH) MNTH_COUNT
FROM
    RP_PRD_CNT
GROUP BY
   NM_PRD_RP
   ,NM_CLD
;
--------------------------------------------------------------------------------
-- End Create VW_RP_MNTH_COUNT                                                --
--------------------------------------------------------------------------------






--------------------------------------------------------------------------------
-- INVENTORY PERFORMANCE MEASURE SAMPLE QUERIES AND VIEWS                     --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Inventory Velocity Sample Query                                            --
--------------------------------------------------------------------------------
-- This sample query summarizes inventory movement in to and out of a         --
-- BusinessUnit for a reporting period to present an aggregate inventory      --
-- velocity measure. This is the kind of metric that might show up on a       --
-- store manager's dashboard.  It provides an indicator of inventory          --
-- movement.                                                                  --
--                                                                            --
-- This sample only considers item movement in UNITS.  This is to simplify    --
-- the details for this illustration.  Typically, retailers will look at      --
-- inventory velocity in UNITS and MONETARY value.                            --
--------------------------------------------------------------------------------
--drop view VW_DW3_BSN_RP_INV_VLCTY;
create view VW_DW3_BSN_RP_INV_VLCTY as
with INVTY_DTL as
    (
        select
            --------------------------------------------------------------------
            -- ARTS most granular level of inventory tracking is by item,     --
            -- business unit, location inside a business unit, inventory      --
            -- state (i.e. damaged, reserved, available for sale) and revenue --
            -- cost center. This fact data consists of beginning and ending   --
            -- unit count balances for a REPORTING PERIOD and cumulative      --
            -- counts for different inventory actions (receipts, sales, etc.).--                                       --
            --------------------------------------------------------------------
             DW3_FACT_INVENTORY.ID_ITM         -- Item ID (SKU)
            ,DW3_FACT_INVENTORY.ID_BSN_UN      -- Business Unit
            ,DW3_FACT_INVENTORY.ID_LCN         -- Location inside Business Unit
            ,DW3_FACT_INVENTORY.ID_ST_INV      -- Inventory State
            ,DW3_FACT_INVENTORY.ID_CTR_RVN_CST -- Revenue Cost Center
            ,DW3_FACT_INVENTORY.ID_PRD_RP      -- Reporting Period
            --------------------------------------------------------------------
            -- Reporting period summary data in our InventoryFact table       --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_BGN         -- Beginning unit count balance
            ,DW3_FACT_INVENTORY.QU_RCV         -- Cumulative Quantity received
            ,DW3_FACT_INVENTORY.QU_TSF_IN      -- Cumulative Transfer In
            ,DW3_FACT_INVENTORY.QU_TSF_OT      -- Cumulative Transfer Out
            ,DW3_FACT_INVENTORY.QU_ADJT        -- Cumulative Adjustment 
            ,DW3_FACT_INVENTORY.QU_RTN         -- CUmulative Customer Returns
            ,DW3_FACT_INVENTORY.QU_SLS         -- Cumulative Sales
            ,DW3_FACT_INVENTORY.QU_RTV         -- Cumulative Return to Vendor
            ,DW3_FACT_INVENTORY.QU_END         -- Ending unit count balance
            -------------------------------------------------------------------
            -- Calculate non-additive facts at most granular level           --
            -------------------------------------------------------------------
            ,(DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2 
              as QU_AVG_INVTY                  -- Average Inventory for Reporting Period
                                               -- which is a NON-ADDITIVE fact
            ,DW3_FACT_INVENTORY.QU_SLS/        
             ((DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2)
              as QU_TRNOVR_INVTY               -- Inventory Turnover 
                                               -- which is a NON-ADDITIVE fact

        from
            DW3_FACT_INVENTORY
    )    
--------------------------------------------------------------------------------
-- Summarization to business unit level using the INVTY_DTL subquery which    --
-- provides the most granular level of detail avaible from the InventoryFact  --
-- table. The inventory balances and summary counts are additive fact in this --
-- sample because they are summed along a NON-reporting period dimension      --
-- (business unit).                                                           --
--------------------------------------------------------------------------------
select
     INVTY_DTL.ID_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.NM_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.TY_BSN_UN
    ,INVTY_DTL.ID_PRD_RP
    ,SUM(INVTY_DTL.QU_BGN)    as QU_BGN
    ,SUM(INVTY_DTL.QU_RCV)    as QU_RCV
    ,SUM(INVTY_DTL.QU_TSF_IN) as QU_TSF_IN
    ,SUM(INVTY_DTL.QU_TSF_OT) as QU_TSF_OT
    ,SUM(INVTY_DTL.QU_ADJT)   as QU_ADJT
    ,SUM(INVTY_DTL.QU_RTN)    as QU_RTN
    ,SUM(INVTY_DTL.QU_SLS)    as QU_SLS
    ,SUM(INVTY_DTL.QU_RTV)    as QU_RTV
    ,SUM(INVTY_DTL.QU_END)    as QU_END
    ----------------------------------------------------------------------------
    -- Recalculate average inventory and inventory turnover for BUSINESS      --
    -- UNIT since the detailed calculations are NON-ADDITIVE facts. We use    --
    -- business unit summary facts to do this calculation.                    --
    ----------------------------------------------------------------------------
    ,(SUM(INVTY_DTL.QU_BGN) + SUM(INVTY_DTL.QU_END))/2 as QU_BSN_UN_AVG_INVTY
    ,SUM(INVTY_DTL.QU_SLS) /
     ((SUM(INVTY_DTL.QU_BGN) + SUM(INVTY_DTL.QU_END))/2)as QU_BSN_UN_TRNOVR_INVTY
    
from
    INVTY_DTL
    join DW3_DIM_BUSINESS_UNIT
    on INVTY_DTL.ID_BSN_UN =  DW3_DIM_BUSINESS_UNIT.ID_BSN_UN
group by
     INVTY_DTL.ID_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.NM_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.TY_BSN_UN
    ,INVTY_DTL.ID_PRD_RP
;
--------------------------------------------------------------------------------
-- END Inventory Velocity Sample Query                                        --
--------------------------------------------------------------------------------



--<<<  START HERE >>>
--------------------------------------------------------------------------------
-- Inventory Velocity Units and Cost Sample Query by Business Unit Reporting  --
-- Period                                                                     --
--------------------------------------------------------------------------------
-- This sample view summarizes inventory movement in to and out of a          --
-- BusinessUnit to present an aggregate inventory velocity measure. This is   --
-- the kind of metric that might show up on a store manager's dashboard.  It  --
-- provides an indicator of inventory movement.  It tells the manager to look -- 
-- deeper if inventory movement is below or above an expected value. To look  --
-- deeper additional, more detailed summary query drilling down to the        --
-- individual item level.                                                     --
--                                                                            --
--------------------------------------------------------------------------------
--drop view VW_DW3_BSN_RP_INV_VLCTY_CST;
create view VW_DW3_BSN_RP_INV_VLCTY_CST as
with INVTY_DTL as 
    (
        select
            --------------------------------------------------------------------
            -- ARTS most granular level of inventory tracking is by item,     --
            -- business unit, location inside a business unit, inventory      --
            -- state (i.e. damaged, reserved, available for sale) and revenue --
            -- cost center.  This fact data consists of beginning and ending  --
            -- unit count balances for a REPORTING PERIOD and cumulative      --
            -- counts for different inventory actions (receipts, sales, etc.).--
            --------------------------------------------------------------------
             DW3_FACT_INVENTORY.ID_ITM         -- Item ID (SKU)
            ,DW3_FACT_INVENTORY.ID_BSN_UN      -- Business Unit
            ,DW3_FACT_INVENTORY.ID_LCN         -- Location inside Business Unit
            ,DW3_FACT_INVENTORY.ID_ST_INV      -- Inventory State
            ,DW3_FACT_INVENTORY.ID_CTR_RVN_CST -- Revenue Cost Center
            ,DW3_FACT_INVENTORY.ID_PRD_RP      -- Reporting Period
            --------------------------------------------------------------------
            -- Reporting period summary data in our InventoryFact table       --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_BGN         -- Beginning unit count balance
            ,DW3_FACT_INVENTORY.QU_RCV         -- Cumulative Quantity received
            ,DW3_FACT_INVENTORY.QU_TSF_IN      -- Cumulative Transfer In
            ,DW3_FACT_INVENTORY.QU_TSF_OT      -- Cumulative Transfer Out
            ,DW3_FACT_INVENTORY.QU_ADJT        -- Cumulative Adjustment 
            ,DW3_FACT_INVENTORY.QU_RTN         -- CUmulative Customer Returns
            ,DW3_FACT_INVENTORY.QU_SLS         -- Cumulative Sales
            ,DW3_FACT_INVENTORY.QU_RTV         -- Cumulative Return to Vendor
            ,DW3_FACT_INVENTORY.QU_END         -- Ending unit count balance
            --------------------------------------------------------------------
            -- Reporting Period Inventory Cost of inventory moved             --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_BGN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN
                as CP_BGN                      -- Beginning Inventory AT cost
            ,DW3_FACT_INVENTORY.TC_RCV_CM      -- Total cost of receipts based on
                                               -- actual receiving item costs 
            --------------------------------------------------------------------
            -- The following inventory movement costs use the reporting       --
            -- period ending average unit cost  			                  --
            --------------------------------------------------------------------             
            ,DW3_FACT_INVENTORY.QU_TSF_IN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_TSF_IN                   -- Transfer in cost 

            ,DW3_FACT_INVENTORY.QU_TSF_OT *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_TSF_OT                   -- Transfer out cost

            ,DW3_FACT_INVENTORY.QU_ADJT *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_ADJT                     -- Adjustment cost

            ,DW3_FACT_INVENTORY.QU_RTN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_RTN                      -- Customer return cost
   
            ,DW3_FACT_INVENTORY.QU_SLS *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_SLS                      -- Sales cost (COGS)

            ,DW3_FACT_INVENTORY.QU_RTV *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_RTV                      -- Return to vendor cost

            ,DW3_FACT_INVENTORY.QU_END *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_END                      -- Ending inventory AT cost
 
            --------------------------------------------------------------------
            -- Calculate non-additive UNIT facts at most granular level       --
            --------------------------------------------------------------------
            ,(DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2 
              as QU_AVG_INVTY           -- Average Inventory UNITS for Reporting 
                                        -- Period which is a NON-ADDITIVE fact
            ,DW3_FACT_INVENTORY.QU_SLS/        
             ((DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2)
              as QU_TRNOVR_INVTY        -- Inventory Turnover UNITS
                                        -- which is a NON-ADDITIVE fact
            --------------------------------------------------------------------
            -- Calculate non-additive COST facts at most granular level       --
            -- NOTE: The ODM sums the receipts, transfers, sales and other    --
            -- actions into a reporting period ending balance (QU_END) so     --
            -- we use that in this query to calculate ending inventory cost.  --
            --------------------------------------------------------------------
            ,((DW3_FACT_INVENTORY.QU_BGN * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN) +
                (DW3_FACT_INVENTORY.QU_END * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END))/ 2
                as CP_AVG_INVTY        -- Average Inventory COST for Reporting
                                       -- Period which is a NON-ADDITIVE fact.
            ,(DW3_FACT_INVENTORY.QU_SLS *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END) /
                (((DW3_FACT_INVENTORY.QU_BGN * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN) +
                (DW3_FACT_INVENTORY.QU_END * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END))/ 2)
                as CP_TRNOVR_INVTY    -- Inventory Turnover COST
                                      -- which is a NON-ADDITIVE fact
        from
            DW3_FACT_INVENTORY
    )    
--------------------------------------------------------------------------------
-- Summarization to business unit level using the INVTY_DTL subquery which    --
-- provides the most granular level of detail avaible from the InventoryFact  --
-- table. The inventory balances and summary counts are additive fact in this --
-- sample because they are summed along a NON-reporting period dimension      --
-- (business unit)                                                            --
--------------------------------------------------------------------------------
select
     INVTY_DTL.ID_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.NM_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.TY_BSN_UN
    ,INVTY_DTL.ID_PRD_RP
    ----------------------------------------------------------------------------
    -- Unit summary (additive)                                                --
    ----------------------------------------------------------------------------
    ,SUM(INVTY_DTL.QU_BGN)     as QU_BGN     -- Beginning Quantity
    ,SUM(INVTY_DTL.QU_RCV)     as QU_RCV     -- Received Quantity
    ,SUM(INVTY_DTL.QU_TSF_IN)  as QU_TSF_IN  -- Transferred In Quantity
    ,SUM(INVTY_DTL.QU_TSF_OT)  as QU_TSF_OT  -- Transferred Out Quantity
    ,SUM(INVTY_DTL.QU_ADJT)    as QU_ADJT    -- Adjusted Quantity
    ,SUM(INVTY_DTL.QU_RTN)     as QU_RTN     -- Customer Return Quantity
    ,SUM(INVTY_DTL.QU_SLS)     as QU_SLS     -- Sold Quantity
    ,SUM(INVTY_DTL.QU_RTV)     as QU_RTV     -- Return to Vendor Quantity
    ,SUM(INVTY_DTL.QU_END)     as QU_END     -- Ending Quantity
    ----------------------------------------------------------------------------
    -- Cost summary (additive)                                                --
    ----------------------------------------------------------------------------
    ,SUM(INVTY_DTL.CP_BGN)     as CP_BGN     -- Beginning cost
    ,SUM(INVTY_DTL.TC_RCV_CM)  as TC_RCV_CM  -- Received cost
    ,SUM(INVTY_DTL.CP_TSF_IN)  as CP_TSF_IN  -- Transfer In Cost
    ,SUM(INVTY_DTL.CP_TSF_OT)  as CP_TFS_OUT -- Transfer Out Cost
    ,SUM(INVTY_DTL.CP_ADJT)    as CP_ADJT    -- Adjustment Cost
    ,SUM(INVTY_DTL.CP_RTN)     as CP_RTN     -- Customer Return Cost Value
    ,SUM(INVTY_DTL.CP_SLS)     as CP_SLS     -- Cost of goods sold
    ,SUM(INVTY_DTL.CP_RTV)     as CP_RTV     -- Return to vendor cost
    ,SUM(INVTY_DTL.CP_END)     as CP_END     -- Ending inventory cost
    ----------------------------------------------------------------------------
    -- Recalculate average inventory and inventory turnover for BUSINESS      --
    -- UNIT reporting period since the detailed calculations are NON-ADDITIVE --
    -- facts. We use business unit level summary facts to do this             --
    --calculation.                                                            --
    ----------------------------------------------------------------------------
    ,(SUM(INVTY_DTL.QU_BGN) + SUM(INVTY_DTL.QU_END))/2 as QU_BSN_UN_AVG_INVTY
    ,SUM(INVTY_DTL.QU_SLS) /
     ((SUM(INVTY_DTL.QU_BGN) + SUM(INVTY_DTL.QU_END))/2)as QU_BSN_UN_TRNOVR_INVTY
     ---------------------------------------------------------------------------
     -- Recalculate average inventory COST and inventory turnover for         --
     -- BUSINESS UNIT reporting period since the detailed calculations are    --
     -- NON-ADDITIVE FACTS.                                                   --
     ---------------------------------------------------------------------------
    ,(SUM(INVTY_DTL.CP_BGN) + SUM(INVTY_DTL.CP_END)) / 2 as CP_BSN_UN_AVG_INVTY
    ,SUM(INVTY_DTL.CP_SLS)/
        ((SUM(INVTY_DTL.CP_BGN) + SUM(INVTY_DTL.CP_END)) / 2) as CP_BSN_UN_TRNOVR_INVTY
from
    INVTY_DTL
    join DW3_DIM_BUSINESS_UNIT
    on INVTY_DTL.ID_BSN_UN =  DW3_DIM_BUSINESS_UNIT.ID_BSN_UN
group by
     INVTY_DTL.ID_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.NM_BSN_UN
    ,DW3_DIM_BUSINESS_UNIT.TY_BSN_UN
    ,INVTY_DTL.ID_PRD_RP
;
--------------------------------------------------------------------------------
-- END Inventory Velocity using COST Sample Query                             --
--------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- Inventory Velocity for a Business Unit across more than one reporting     --
-- period                                                                    --
-------------------------------------------------------------------------------
select
     VW_DW3_BSN_RP_INV_VLCTY_CST.ID_BSN_UN
    ---------------------------------------------------------------------------
    -- Sum data used in multiperiod avg and turnover calc.                   --
    ---------------------------------------------------------------------------
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_BGN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_END)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_SLS)
    ---------------------------------------------------------------------------
    -- Sum data used in multiperiod avg cost and turnover calc.              --
    ---------------------------------------------------------------------------
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_BGN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_END)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_SLS)
    --
    ,COUNT(VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP)
    ---------------------------------------------------------------------------
    -- Sum other counts for multi reporting periods                          --
    ---------------------------------------------------------------------------
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_TSF_IN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_TSF_OT)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_ADJT)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_RTN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_RTV)
    ---------------------------------------------------------------------------
    -- Sum other costs for multi reporting periods                           --
    ---------------------------------------------------------------------------
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_TSF_IN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_TSF_IN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_ADJT)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_RTN)
    ,SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_RTV)
    --------------------------------------------------------------------------
    -- Multiperiod reporting period average inventory unit count            --
    --------------------------------------------------------------------------
    ,(SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_BGN) +
        SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_END)) /
        (COUNT(VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP) + 1)
        as QU_BSN_UN_RP_AVG_INVTY      -- average inventory for the SET of 
                                       -- reporting periods included in the
                                       -- where clause
    --------------------------------------------------------------------------
    -- Multiperiod inventory unit turnover calcualtion                      --
    --------------------------------------------------------------------------
    ,(SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_SLS) /
        (SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_BGN) +
        SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_END))) /
        (COUNT(VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP) + 1)
        as QU_BSN_UN_RP_TRNOVR_INVTY   -- Inventory turnover in units for 
                                       -- the SET of reporting periods included
                                       -- in the where clause
    --------------------------------------------------------------------------
    -- Multiperiod inventory average COST                                   --
    --------------------------------------------------------------------------
    ,(SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_BGN) +
        SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_END)) /
        (COUNT(VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP) + 1)
        as CP_BSN_UN_RP_AVG_INVTY
    --------------------------------------------------------------------------
    -- Multiperiod inventory COST turnover calculation                      --
    --------------------------------------------------------------------------
    ,(SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_SLS) /
        (SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.CP_BGN) +
        SUM(VW_DW3_BSN_RP_INV_VLCTY_CST.QU_END)) /
        (COUNT(VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP) + 1)
        as CP_BSN_UN_RP_TRNOVR_INVTY   -- Inventory turnover in COST for
                                       -- the SET of reporting periods included
                                       -- in the where clause 
from
    VW_DW3_BSN_RP_INV_VLCTY_CST
    join DW3_DIM_CA_PRD_RP
    on VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP =  DW3_DIM_CA_PRD_RP.ID_PRD_RP
    and  DW3_DIM_CA_PRD_RP.ID_CLD = 1          --Using the NRF 4-5-4 calendar
where
    VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP >= 201301 and   
    VW_DW3_BSN_RP_INV_VLCTY_CST.ID_PRD_RP <= 201306
group by
    VW_DW3_BSN_RP_INV_VLCTY_CST.ID_BSN_UN
;
-------------------------------------------------------------------------------
-- END Inventory Velocity for a Business Unit across more than one reporting --
-- period                                                                    --
-------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Sample query to obtain a stock out percentage of out of stock conditions   --
-- encountered by each store for a reporting period. The return rowset is a   --
-- a list of business units (stores) and the percentage of carried items that --
-- that are out of stock for a reporting period.                              --
--------------------------------------------------------------------------------
with INVTY as
    (
        select
             DW3_FACT_INVENTORY.ID_ITM                -- Item ID    
            ,DW3_FACT_INVENTORY.ID_BSN_UN             -- Business Unit
            ,DW3_FACT_INVENTORY.ID_PRD_RP             -- Reporting Period
            ,sum(DW3_FACT_INVENTORY.QU_END) as QU_END -- Ending Onhand Count
            --------------------------------------------------------------------
            -- Next count the out of stock occurences for this item,          --
            -- business unit and reporting period based on our filter         --
            -- criteria defined in the where clause                           --
            --------------------------------------------------------------------
            ,case
                when sum(DW3_FACT_INVENTORY.QU_END) < 1 then 1
                else 0
             end as STK_OUT_IND
        from
            DW3_FACT_INVENTORY
            join DW3_DIM_INVENTORY_LOCATION
            on DW3_FACT_INVENTORY.ID_LCN = DW3_DIM_INVENTORY_LOCATION.ID_LCN

            join CO_ST_INV
            on DW3_FACT_INVENTORY.ID_ST_INV = CO_ST_INV.ID_ST_INV
        ------------------------------------------------------------------------
        -- In this sample we are limiting our selection of items to those     --
        -- with an inventory state of AVAILABLE FOR SALE so we're not looking --
        -- at damaged or reserved balances.  We are also only looking at      --
        -- item on the SALES FLOOR or in the STOCKROOM.  The purpose is we    --
        -- only want to consider merchandise that's saleable.  Retailers      --
        -- may alter this where clause to meet their specific needs.          --
        ------------------------------------------------------------------------
        where  
            CO_ST_INV.NM_ST_INV = 'AVAILABLE_FOR_SALE' 
            and DW3_DIM_INVENTORY_LOCATION.NM_LCN in ('SALES_FLOOR','STOCKROOM')
        group by
             DW3_FACT_INVENTORY.ID_ITM        -- Item ID    
            ,DW3_FACT_INVENTORY.ID_BSN_UN     -- Business Unit
            ,DW3_FACT_INVENTORY.ID_PRD_RP 
    )
select
     INVTY.ID_BSN_UN
    ,INVTY.ID_PRD_RP
    ,COUNT(ID_ITM) as QU_ITM_SKU_CARRIED
    ,SUM(INVTY.STK_OUT_IND) as QU_ITM_STKOUT
    ,ROUND(SUM(INVTY.STK_OUT_IND)/COUNT(ID_ITM),2) as PCT_STKOUT_ITM
from
    INVTY
group by
     INVTY.ID_BSN_UN
    ,INVTY.ID_PRD_RP
--------------------------------------------------------------------------------
-- End of Business Unit Stock out percentage for a reporting period query     --
--------------------------------------------------------------------------------






--------------------------------------------------------------------------------
-- Item Out of Stock Period Count by Item and Business Unit                   -- 
--------------------------------------------------------------------------------
with INVTY as
    (
        select
             DW3_FACT_INVENTORY.ID_ITM                -- Item ID    
            ,DW3_FACT_INVENTORY.ID_BSN_UN             -- Business Unit
            ,DW3_FACT_INVENTORY.ID_PRD_RP             -- Reporting Period
            ,sum(DW3_FACT_INVENTORY.QU_END) as QU_END -- Ending Onhand Count
            --------------------------------------------------------------------
            -- Next count the out of stock occurences for this item,          --
            -- business unit and reporting period based on our filter         --
            -- criteria defined in the where clause                           --
            --------------------------------------------------------------------
            ,case
                when sum(DW3_FACT_INVENTORY.QU_END) < 1 then 1
                else 0
             end as STK_OUT_IND
        from
            DW3_FACT_INVENTORY
            join DW3_DIM_INVENTORY_LOCATION
            on DW3_FACT_INVENTORY.ID_LCN = DW3_DIM_INVENTORY_LOCATION.ID_LCN

            join CO_ST_INV
            on DW3_FACT_INVENTORY.ID_ST_INV = CO_ST_INV.ID_ST_INV
          ----------------------------------------------------------------------
          -- Establish the range of reporting periods to cover for the        --
          -- stockout occurence count                                         --
          ----------------------------------------------------------------------
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_INVENTORY.ID_PRD_RP = DW3_DIM_CA_PRD_RP.ID_PRD_RP
            and  DW3_DIM_CA_PRD_RP.ID_CLD = 1 
        ------------------------------------------------------------------------
        -- In this sample we are limiting our selection of items to those     --
        -- with an inventory state of AVAILABLE FOR SALE so we're not looking --
        -- at damaged or reserved balances.  We are also only looking at      --
        -- item on the SALES FLOOR or in the STOCKROOM.  The purpose is we    --
        -- only want to consider merchandise that's saleable.  Retailers      --
        -- may alter this where clause to meet their specific needs.          --
        ------------------------------------------------------------------------
        where  
            CO_ST_INV.NM_ST_INV = 'AVAILABLE_FOR_SALE' 
            and DW3_DIM_INVENTORY_LOCATION.NM_LCN in ('SALES_FLOOR','STOCKROOM')
            
            and DW3_FACT_INVENTORY.ID_PRD_RP >= 201301 
            and DW3_FACT_INVENTORY.ID_PRD_RP <= 201306

        group by
             DW3_FACT_INVENTORY.ID_ITM        -- Item ID    
            ,DW3_FACT_INVENTORY.ID_BSN_UN     -- Business Unit
            ,DW3_FACT_INVENTORY.ID_PRD_RP 
    )
select
     INVTY.ID_BSN_UN                    -- Business Unit
    ,INVTY.ID_ITM                       -- Item
    ,SUM(STK_OUT_IND) as QU_RP_STKOUT   -- Count of stock out reporting periods
from
    INVTY
group by
     INVTY.ID_BSN_UN
    ,INVTY.ID_ITM
;
--------------------------------------------------------------------------------
-- END Item Out of Stock Period Count by Item and Business Unit               -- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sample inventory freshness query - This sample has arbitray logic to       --
-- determine "freshness".  This query can be upgraded to incorporate          --
-- freshness condition day count ranges that vary by product category and     --
-- other item attributes.                                                     --
--------------------------------------------------------------------------------
with INVTY as
    (
        select
             DW3_FACT_INVENTORY.ID_ITM
            ,DW3_FACT_INVENTORY.ID_BSN_UN
            ,DW3_FACT_INVENTORY.ID_PRD_RP
            ,DW3_FACT_INVENTORY.DC_INV_FS_RCPT
            ,DW3_FACT_INVENTORY.DC_INV_LS_RCPT
            ,case
                when DW3_FACT_INVENTORY.DC_INV_LS_RCPT is null then
                    DATEDIFF(DD,DW3_FACT_INVENTORY.DC_INV_FS_RCPT,CURRENT_TIMESTAMP)
                else
                    DATEDIFF(DD,DW3_FACT_INVENTORY.DC_INV_LS_RCPT,CURRENT_TIMESTAMP)
             end as DY_CNT_SINCE_LST_RCPT
        from
            DW3_FACT_INVENTORY
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_INVENTORY.ID_PRD_RP = DW3_DIM_CA_PRD_RP.ID_PRD_RP
            and  DW3_DIM_CA_PRD_RP.ID_CLD = 1 

            join DW3_DIM_INVENTORY_LOCATION
            on DW3_FACT_INVENTORY.ID_LCN = DW3_DIM_INVENTORY_LOCATION.ID_LCN

            join CO_ST_INV
            on DW3_FACT_INVENTORY.ID_ST_INV = CO_ST_INV.ID_ST_INV
        where
           ---------------------------------------------------------------------
           -- In this sample we're limiting freshness to one reporting period --
           -- and the inventory state to items that are available for sale    -- 
           -- and are on the sales floor or in the stock room.                --
           ---------------------------------------------------------------------
            DW3_FACT_INVENTORY.ID_PRD_RP = 201301 
            and CO_ST_INV.NM_ST_INV = 'AVAILABLE_FOR_SALE' 
            and DW3_DIM_INVENTORY_LOCATION.NM_LCN in ('SALES_FLOOR','STOCKROOM')
			and DW3_FACT_INVENTORY.DC_INV_FS_RCPT is not null 
    )            
select
     INVTY.ID_ITM
    ,INVTY.ID_BSN_UN
    ,INVTY.ID_PRD_RP
    ,INVTY.DY_CNT_SINCE_LST_RCPT
    ,case
         when INVTY.DY_CNT_SINCE_LST_RCPT between 0 and 3 then 'FRESH'
         when INVTY.DY_CNT_SINCE_LST_RCPT between 4 and 7 then 'OUT_OF_DATE'
         when INVTY.DY_CNT_SINCE_LST_RCPT > 7 then 'UNSALABLE'
     end as SAMPLE_FRESHNESS_VALUE
from
    INVTY
--------------------------------------------------------------------------------
-- END Sample inventory freshness query                                       --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- GMROI Sample Query                                                         --
--------------------------------------------------------------------------------
with INVTY_DTL as 
    (
        ------------------------------------------------------------------------
        -- Calculate intermediate values used for GMROI.  The values derived  --
        -- in this subquery are by item, business unit and reporting period.  --
        -- Retailers have to be careful in choosing a reporting period        --
        -- for INVENTORY to ensure it is at the appropriate level of          --
        -- granularity so it can be summarized into higher level reporting    --
        -- periods (which are composed of the lower level reporting periods)  --
        --                                                                    --
        -- Note that we are extending the INVTY_DTL subquery pattern          --
        -- established in earlier samples.  This is so users can pick up a    --
        -- single consolidated INVTY_DTL subquery that supports a number of   --
        -- outer sample queries.  The intent is to prepare the detail         --
        -- inventory facts for subsequent summarization and analysis along    --
        -- different dimensions.  Keep in mind that this subquery is working  --
        -- at the business unit, item, reporting period level.                --
        ------------------------------------------------------------------------
        select
            --------------------------------------------------------------------
            -- ARTS most granular level of inventory tracking is by item,     --
            -- business unit, location inside a business unit, inventory      --
            -- state (i.e. damaged, reserved, available for sale) and revenue --
            -- cost center.  This fact data consists of beginning and ending  --
            -- unit count balances for a REPORTING PERIOD and cumulative      --
            -- counts for different inventory actions (receipts, sales, etc.).--
            --------------------------------------------------------------------
             DW3_FACT_INVENTORY.ID_ITM         -- Item ID (SKU)
            ,DW3_FACT_INVENTORY.ID_BSN_UN      -- Business Unit
            ,DW3_FACT_INVENTORY.ID_LCN         -- Location inside Business Unit
            ,DW3_FACT_INVENTORY.ID_ST_INV      -- Inventory State
            ,DW3_FACT_INVENTORY.ID_CTR_RVN_CST -- Revenue Cost Center
            ,DW3_FACT_INVENTORY.ID_PRD_RP      -- Reporting Period
            --------------------------------------------------------------------
            -- Reporting period summary data in our InventoryFact table       --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_BGN         -- Beginning unit count balance
            ,DW3_FACT_INVENTORY.QU_RCV         -- Cumulative Quantity received
            ,DW3_FACT_INVENTORY.QU_TSF_IN      -- Cumulative Transfer In
            ,DW3_FACT_INVENTORY.QU_TSF_OT      -- Cumulative Transfer Out
            ,DW3_FACT_INVENTORY.QU_ADJT        -- Cumulative Adjustment 
            ,DW3_FACT_INVENTORY.QU_RTN         -- CUmulative Customer Returns
            ,DW3_FACT_INVENTORY.QU_SLS         -- Cumulative Sales
            ,DW3_FACT_INVENTORY.QU_RTV         -- Cumulative Return to Vendor
            ,DW3_FACT_INVENTORY.QU_END         -- Ending unit count balance
            --------------------------------------------------------------------
            -- Reporting Period Inventory Cost of inventory moved             --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_BGN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN
                as CP_BGN                      -- Beginning Inventory AT cost
            ,DW3_FACT_INVENTORY.TC_RCV_CM      -- Total cost of receipts based on
                                               -- actual receiving item costs 
            --------------------------------------------------------------------
            -- The following inventory movement costs use the reporting       --
            -- period ending average unit cost  			                  --
            --------------------------------------------------------------------             
            ,DW3_FACT_INVENTORY.QU_TSF_IN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_TSF_IN                   -- Transfer in cost 

            ,DW3_FACT_INVENTORY.QU_TSF_OT *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_TSF_OT                   -- Transfer out cost

            ,DW3_FACT_INVENTORY.QU_ADJT *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_ADJT                     -- Adjustment cost

            ,DW3_FACT_INVENTORY.QU_RTN *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_RTN                      -- Customer return cost
   
            ,DW3_FACT_INVENTORY.QU_SLS *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_SLS                      -- Sales cost (COGS)

            ,DW3_FACT_INVENTORY.QU_RTV *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_RTV                      -- Return to vendor cost

            ,DW3_FACT_INVENTORY.QU_END *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END
                as CP_END                      -- Ending inventory AT cost
            --------------------------------------------------------------------
            -- GMROI Intermediate Detail Calculations                         --
            --------------------------------------------------------------------
            ,DW3_FACT_INVENTORY.QU_SLS -
                DW3_FACT_INVENTORY.QU_RTN
                as QU_NET_UN_SLS               -- Net sale units


            ,DW3_FACT_INVENTORY.MO_UN_RTL      -- Reporting period current unit
                                               -- retail price
            --------------------------------------------------------------------
            -- Calculation using counts and current retail - superceded by    --
            -- using the cumulative gross sales and returns in the next       --
            -- column specification                                           --
            --------------------------------------------------------------------
            --,(DW3_FACT_INVENTORY.QU_SLS -
            --      DW3_FACT_INVENTORY.QU_RTN ) *
            --	  DW3_FACT_INVENTORY.MO_UN_RTL
            --	  as MO_NET_SLS              -- Net sale amount at retail
            
            ,(DW3_FACT_INVENTORY.TP_SLS_GS_CM -
                 DW3_FACT_INVENTORY.TP_RTN)
                 as MO_NET_SLS               -- Monetary Net Sales Amount

            ,(DW3_FACT_INVENTORY.TP_SLS_GS_CM -
                 DW3_FACT_INVENTORY.TP_RTN) -
                 ((DW3_FACT_INVENTORY.QU_SLS -
                  DW3_FACT_INVENTORY.QU_RTN) *
                  DW3_FACT_INVENTORY.CP_UN_AV_WT_END)
                  as MO_GRS_MRGN             -- Monetary gross margin amount

            ,(DW3_FACT_INVENTORY.QU_BGN *
                 DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN)/
                 (DW3_FACT_INVENTORY.TP_SLS_GS_CM -
                 DW3_FACT_INVENTORY.TP_RTN)
                 as PD_BGN_SLS_STK_RATIO       -- Period beginning sales to 
                                               -- stock ratio.  
            -- Gross Margin Percent
            ,(DW3_FACT_INVENTORY.TP_SLS_GS_CM -
                 DW3_FACT_INVENTORY.TP_RTN) -
                 ((DW3_FACT_INVENTORY.QU_SLS -
                  DW3_FACT_INVENTORY.QU_RTN) *
                  DW3_FACT_INVENTORY.CP_UN_AV_WT_END)/
                  (DW3_FACT_INVENTORY.TP_SLS_GS_CM -
                  DW3_FACT_INVENTORY.TP_RTN)
                  as PCT_GRS_MRGN               -- Gross Margin Percent
            --------------------------------------------------------------------
            -- END GMROI Detailed Calculations                                --		  
            --------------------------------------------------------------------

            --------------------------------------------------------------------
            -- Calculate non-additive UNIT facts at most granular level       --
            --------------------------------------------------------------------
            ,(DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2 
              as QU_AVG_INVTY           -- Average Inventory UNITS for Reporting 
                                        -- Period which is a NON-ADDITIVE fact
            ,DW3_FACT_INVENTORY.QU_SLS/        
             ((DW3_FACT_INVENTORY.QU_BGN +
              DW3_FACT_INVENTORY.QU_END)/2)
              as QU_TRNOVR_INVTY        -- Inventory Turnover UNITS
                                        -- which is a NON-ADDITIVE fact
            --------------------------------------------------------------------
            -- Calculate non-additive COST facts at most granular level       --
            -- NOTE: The ODM sums the receipts, transfers, sales and other    --
            -- actions into a reporting period ending balance (QU_END) so     --
            -- we use that in this query to calculate ending inventory cost.  --
            --------------------------------------------------------------------
            ,((DW3_FACT_INVENTORY.QU_BGN * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN) +
                (DW3_FACT_INVENTORY.QU_END * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END))/ 2
                as CP_AVG_INVTY        -- Average Inventory COST for Reporting
                                       -- Period which is a NON-ADDITIVE fact.
            ,(DW3_FACT_INVENTORY.QU_SLS *
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END) /
                (((DW3_FACT_INVENTORY.QU_BGN * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_BGN) +
                (DW3_FACT_INVENTORY.QU_END * 
                DW3_FACT_INVENTORY.CP_UN_AV_WT_END))/ 2)
                as CP_TRNOVR_INVTY    -- Inventory Turnover COST
                                      -- which is a NON-ADDITIVE fact
        from
            DW3_FACT_INVENTORY

            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_INVENTORY.ID_PRD_RP = DW3_DIM_CA_PRD_RP.ID_PRD_RP
            and  DW3_DIM_CA_PRD_RP.ID_CLD = 1 

            join DW3_DIM_INVENTORY_LOCATION
            on DW3_FACT_INVENTORY.ID_LCN = DW3_DIM_INVENTORY_LOCATION.ID_LCN

            join CO_ST_INV
            on DW3_FACT_INVENTORY.ID_ST_INV = CO_ST_INV.ID_ST_INV
        where
           ---------------------------------------------------------------------
           -- In this sample we're limiting GMROI to the inventory state for  --
           -- items that are available for sale and are on the sales floor or --
           -- in the stock room.                                              --
           ---------------------------------------------------------------------
            CO_ST_INV.NM_ST_INV = 'AVAILABLE_FOR_SALE' 
            and DW3_DIM_INVENTORY_LOCATION.NM_LCN in ('SALES_FLOOR','STOCKROOM')
            and DW3_FACT_INVENTORY.DC_INV_FS_RCPT is not null 
 
     )    
--------------------------------------------------------------------------------
-- Main query to calculate the GMROI for each business unit and item returned --
-- from the subquery.  Keep in mind we filtered the rows to only look at      --
-- items available for sale and items on the sales floor and stock room.      --
--------------------------------------------------------------------------------
select
     INVTY_DTL.ID_ITM             -- Item ID
    ,INVTY_DTL.ID_BSN_UN          -- Business Unit
    ,COUNT(ID_PRD_RP)            -- Number of contiguous reporting periods
    ,SUM(INVTY_DTL.CP_END)      
        /(COUNT(ID_PRD_RP) + 1)
        as MO_RP_AVG_INVTY_CST    -- Average Inventory Cost per Reporting Period
    ,SUM(INVTY_DTL.MO_NET_SLS)/
        (SUM(INVTY_DTL.CP_END)/(COUNT(ID_PRD_RP) + 1))
        as ITM_BSN_GMROI          -- GMROI per item, business unit, for the 
                                  -- contiguous set of reporting periods
from
    INVTY_DTL
where
    INVTY_DTL.ID_PRD_RP between 201301 and 201306  -- Sample reporting period ID's
group by
     INVTY_DTL.ID_ITM
    ,INVTY_DTL.ID_BSN_UN

;
--------------------------------------------------------------------------------
-- END GMROI Sample Query                                                     --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Time Dimension sub query design                                            --
--------------------------------------------------------------------------------
-- We need a way to tie a DC_DY_BSN which is part of each sale return line    --
-- item to a reporting period and one or more calendar periods within that    -- 
-- reporting period.                                                          --
--                                                                            --
-- There are some rules we need to follow. First is that any of the calendar  --
-- periods (which may be at different levels) must fall within the reporting  --
-- period.  Second a given reporting period may be linked to one and only     --
-- one calendar. Third a reporting period may consist of N calendar periods   --
-- but those calendar period must be at the same level. This means that a     --
-- reporting period can consist of N calendar months or M calendar days but   --
-- cannot mix calendar period types.  The most flexible way to define a       --
-- reporting period is at the calendar day level.                             --
--                                                                            --
-- Parameters that must be provided to use the time dimension tables:         --
--     - Reporting Period(s) - The reporting period or periods must be        --
--                             identified in order to pull the correct detail --
--                             facts.  Keep in mind that a reporting period   --
--                             always resolves to one or more business day    --
--                             dates and that transactions are always         --
--                             assigned to a business day date.               --
--                                                                            --
--     - Calendar ID - Both calendar periods and reporting periods are tied   --
--                     to a specific calendar which must be identified.       --
--                                                                            --
--     - Calendar Level  - Reporting periods may incorporate one or more      --
--                         calendar periods.  The calender period level must  --
--                         be designated (e.g. month, day, week, etc.  Note   --
--                         that a reporting period must contain one or more   --
--                         contiguous calendar periods and those calendar     --
--                         periods.  This means that the lowest, most         --
--                         granular reporting period is a calendar day        --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with ReportingPeriod as
    (
	    select
		     DC_DY_BSN
            ,ID_CLD
			,NM_CLD
			,ID_PRD_RP
			,NM_PRD_RP
			,ID_CLD_PRD
			,datename(mm,DC_DY_BSN) NM_DT 
			,datepart(dw,DC_DY_BSN) DT_PT
    -- Need a function to disassemble the PATH values using '|' and derive
	-- different columns
	--		,datename(dw,DC_DY_BSN) 
	--		,CA_PRD_LVL_NM_PTH
			,dbo.FN_REVSPLIT(CA_PRD_LVL_NM_PTH,'|') as LVL_PTH
	        
	--		,CA_PRD_NM_PTH
			,dbo.FN_REVSPLIT(CA_PRD_NM_PTH,'|') as NM_PTH
        from
		    VW_DW3_DIM_CA_PRD_RP                   -- take the "VW_' away later this is for testing only.
        where
		    NM_CLD = 'NRF 4-5-4 Retail Calendar'   -- sample calendar parameter
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'		
			and ID_CLD_PRD between 174 and 179	
	)

select 
     DC_DY_BSN
	,ID_CLD
	,NM_CLD
	,ID_PRD_RP
	,NM_PRD_RP
	,LVL_PTH
	,NM_PTH
    ,dbo.FN_RP_CP_EXTR(LVL_PTH,NM_PTH,'|','PERIOD') as CP_SUM

from 
    ReportingPeriod



--------------------------------------------------------------------------------
-- Function: FN_RP_CP_EXTR2 - Extract Calendar Period Lable and Name from     --
--           concatenated calendar period hierarchy column                    --
--                                                                            --
--           This is version 1 of this function.  It will be replaced in      --
--           future ARTS DWM releases.  It is limited to yeras, seasons,      --
--           quarters and periods (months) and weeks for the NRF 4-5-4        --
--           calendar.                                                         --
-- Change Log                                                                 --
-- ----------                                                                 --
-- 2013-08-27 TFS  Added calls to FN_REVSPLIT to reorder the calendar         --
--                 periods so they follow YR-SEASON-QUARTER-PERIOD-WEEK_DAY   --
--                 left to right pattern.                                     --
-- 2013-08-27 TFS  Added week to the periods returned                         --
--------------------------------------------------------------------------------
-- Input Param: Level Path - calendar level labels (e.g. YEAR, QUARTER, etc.) --
--              Level Name - calendar level names (e.g. 2013, 1, etc.)        --
--              Delimiter - character used to separate values we use a '|'    --
--              Return Calendar Level - Tells Function what set of calendar   --
--                                      hierarchy labels and values to return --
--                                                                            --
--    Value of Return Calendar Level (@return_ca_lvl):                        --
--    ------------------------------------------------                        --
--   'WEEK'    returns <calendar year label>: <calendar year name > + ' ' +   --
--                     <calendar period label>:<calender week name>           --
--                                                                            --
--   'PERIOD'  returns <calendar year label>:<calendar year name> + ' ' +     --
--                     <calendar period label>:<calendar period name>         --
--                                                                            --
--   'SEASON'  returns <calendar year label>:<calendar year name> + ' ' +     --
--                     <calendar season label>:<calender season name>         --
--                                                                            --
--   'QUARTER' returns <calendar year label>:<calendar year name> + ' ' +     --
--                     <calendar quarter label>:<calendar quarter name>       --
--   'YEAR' returns <calendar year label>:<calendar year name>                --
--------------------------------------------------------------------------------
--drop function    dbo.FN_RP_CP_EXTR2;
create function  [dbo].[FN_RP_CP_EXTR2]
    (
       @lvl_pth as varchar(4000)
      ,@lvl_nm as varchar(4000)
      ,@delimiter as char(1)
      ,@return_ca_lvl as varchar(20)
    )
    returns varchar(4000) as

BEGIN
    ----------------------------------------------------------------------------
    -- Return string area where all of the pieces are assembled and returned  --
    -- from this function                                                     --
    ----------------------------------------------------------------------------
    Declare @StrWorkArea as varchar(4000)=''

    ----------------------------------------------------------------------------
    -- Calendar level name variables                                          --
    ----------------------------------------------------------------------------
    Declare @StrYearNm as char(4)=''
    Declare @StrSeasonNm as varchar(255)=''
    Declare @StrQtrNm as varchar(255)=''
    Declare @StrPrdNm as varchar(255)=''
	Declare @StrWkNm as varchar(255)=''
    ----------------------------------------------------------------------------
    -- Calendar level label variables                                         --
    ----------------------------------------------------------------------------
    Declare @strYearLvl as varchar(255)=''
    Declare @strSeasonLvl as varchar(255)=''
    Declare @strQtrLvl as varchar(255)=''
    Declare @strPrdLvl as varchar(255)=''
	Declare @strWkLvl as varchar(255)=''
    ----------------------------------------------------------------------------
    -- Pointer variables used in charindex and substring functions            --
    ----------------------------------------------------------------------------
    Declare @start as integer=0
    Declare @nextdelimit as integer=0
    ----------------------------------------------------------------------------
    -- Length variables for input calendar hierarchy labels and name strings  --
    -- used to test for end of decomposition of character strings             --
    ----------------------------------------------------------------------------    
    Declare @lvl_nm_lth as integer=0
    Declare @lvl_pth_lth as integer=0
    ----------------------------------------------------------------------------
    -- Set string lengths for calendar level names and labels                 --
    ----------------------------------------------------------------------------
    set @lvl_nm_lth = len(@lvl_nm)
    set @lvl_pth_lth = len(@lvl_pth)

	set @lvl_pth = dbo.FN_REVSPLIT(@lvl_pth,'|')  -- Left to right reorder
	set @lvl_nm = dbo.FN_REVSPLIT(@lvl_nm,'|')    -- Left to right reorder

    set @start = 0
    BEGIN
       -------------------------------------------------------------------------
       -- Separate out the NAME parts of the calendar period                  --
       -------------------------------------------------------------------------
       -- Year                                                                --
       -------------------------------------------------------------------------
    
	   set @nextdelimit = charindex(@delimiter,@lvl_nm,@start)
       If @nextdelimit = 0 goto EndOfTextString
       set @StrYearNm = substring(@lvl_nm,@start,@nextdelimit)
       -------------------------------------------------------------------------
       -- Season                                                              --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_nm,@start)
       If @nextdelimit = 0 
           BEGIN
               set @StrSeasonNm = substring(@lvl_nm,@start,@lvl_nm_lth - @start + 1)
               goto EndOfTextString
           END
       set @StrSeasonNm = substring(@lvl_nm,@start,@nextdelimit - @start)
       -------------------------------------------------------------------------
       -- Quarter                                                             --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_nm,@start)
       If @nextdelimit = 0 
           BEGIN
               set @StrQtrNm = substring(@lvl_nm,@start,@lvl_nm_lth - @start + 1)
               goto EndOfTextString
           END
       set @StrQtrNm = substring(@lvl_nm,@start,@nextdelimit - @start)
       -------------------------------------------------------------------------
       -- Period                                                              --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_nm,@start)
       If @nextdelimit = 0 
           BEGIN
               set @strPrdNm = substring(@lvl_nm,@start,@lvl_nm_lth - @start + 1)
               goto EndOfTextString   -- Here's the problem
           END
       set @strPrdNm = substring(@lvl_nm,@start,@nextdelimit - @start)

       -------------------------------------------------------------------------
	   -- Week                                                                --
	   -------------------------------------------------------------------------
	   set @start = @nextdelimit + 1
	   set @nextdelimit = charindex(@delimiter,@lvl_nm,@start)
	   if @nextdelimit = 0
	       BEGIN
		       set @strWkNm = substring(@lvl_nm,@start,@lvl_nm_lth - @start +1)
			   goto LookAtLevel
	       END
	   set @strWkNm = substring(@lvl_nm,@start,@nextdelimit - @start +1)
	   	   
	   -------------------------------------------------------------------------
       -- Separate out LEVEL label                                            --
       -------------------------------------------------------------------------

       LookAtLevel:
       -------------------------------------------------------------------------
       -- Year Level Label                                                    --
       -------------------------------------------------------------------------
       set @start = 1
       set @nextdelimit = charindex(@delimiter,@lvl_pth,@start)
       If @nextdelimit = 0 goto EndOfTextString
       set @strYearLvl = substring(@lvl_pth,@start,@nextdelimit - 1) -- You may want to subtract 1 test and see

       -------------------------------------------------------------------------
       -- Season Level Label                                                  --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_pth,@start)
       if @nextdelimit = 0
           BEGIN
               set @strSeasonLvl = substring(@lvl_pth,@start,@lvl_pth_lth - @start + 1)
               goto EndOfTextString
           END
       set @strSeasonLvl = substring(@lvl_pth,@start,@nextdelimit - @start)

       -------------------------------------------------------------------------
       -- Quarter Level Label                                                 --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_pth,@start)
       if @nextdelimit = 0
           BEGIN
               set @strQtrLvl = substring(@lvl_pth,@start,@lvl_pth_lth - @start + 1)
               goto EndOfTextString
           END
       set @strQtrLvl = substring(@lvl_pth,@start,@nextdelimit - @start)

       -------------------------------------------------------------------------
       -- Period (month) Level Label                                          --
       -------------------------------------------------------------------------
       set @start = @nextdelimit + 1
       set @nextdelimit = charindex(@delimiter,@lvl_pth,@start)
       if @nextdelimit = 0
           BEGIN
               set @strPrdLvl = substring(@lvl_pth,@start,@lvl_pth_lth - @start + 1)
               goto EndOfTextString
           END
       set @strPrdLvl = substring(@lvl_pth,@start,@nextdelimit - @start)

       -------------------------------------------------------------------------
	   -- Week Level Label                                                    --
	   -------------------------------------------------------------------------
	   set @start = @nextdelimit +1
	   set @nextdelimit = charindex(@delimiter,@lvl_pth,@start)
	   if @nextdelimit = 0
	       BEGIN
		       set @strWkLvl = substring(@lvl_pth,@start,@lvl_pth_lth - @start +1)
			   goto EndOfTextString
		   END
       set @strWkLvl = substring(@lvl_pth,@start,@nextdelimit - @start + 1)


       EndOfTextString:   -- You've run out of delimiters so stop

    END
    ----------------------------------------------------------------------------
    -- At this point we've decomposed the calendar level into separate level  --
    -- name and level label components.  Now we will evaluate the             --
    -- @return_ca_lvl input parameter which tells us what parts to reassemble --
    -- and return.  In this sample, we're returning calendar year and the     --
    -- relative NRF period in that year.                                      --
    --                                                                        --
    -- We will generalize this query in future releases.                      --
    ----------------------------------------------------------------------------

    ----------------------------------------------------------------------------
    -- TEST CODE to reassemble all parts of the calendar level down to period --
    ----------------------------------------------------------------------------
    --set @StrWorkArea = @StrYearLvl + ': ' + 
    --  @StrYearNm + ' ' + @StrSeasonLvl + ': ' + 
    --	@StrSeasonNm + ' ' +@StrQtrLvl + ': ' +  
    --	@StrQtrNm  + ' ' +
    --  @strPrdLvl + ': ' + @strPrdNm 	

    ----------------------------------------------------------------------------
    -- Build the @StrWorkArea based on the options selected by the calling    --
    -- logic.  Note we return YEAR plus the requested period                  --
    ----------------------------------------------------------------------------
    set @StrWorkArea =
        case 
		    when @return_ca_lvl = 'WEEK' then
			    @strYearLvl + ':' + @strYearNm + ' ' +
				@strWkLvl + ':' + @strWkNm
		 
            when @return_ca_lvl = 'PERIOD' then
                @strYearLvl + ':' + @strYearNm + ' ' +
                @strPrdLvl + ':' + @strPrdNm

            when @return_ca_lvl = 'QUARTER' then
                @strYearLvl + ':' + @strYearNm + ' ' +
                @StrQtrLvl + ':' + @strQtrNm 
            
            when @return_ca_lvl = 'SEASON' then
                @strYearLvl + ':' + @strYearNm + ' ' +
                @strSeasonLvl + ':' + @strSeasonNm

            when @return_ca_lvl = 'YEAR' then 
                @strYearLvl + ':' + @strYearNm

            else 'Invalid calendar level requested'
    end 
    return @StrWorkArea
END
;
--------------------------------------------------------------------------------
-- End Function: FN_RP_CP_EXTR2                                                --
--------------------------------------------------------------------------------



select dbo.FN_RP_CP_EXTR ('YEAR|SEASON|QUARTER|PERIOD','2013|SPRING|2|5','|','YEAR');
--------------------------------------------------------------------------------
-- End of test function execution                                             --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Back to the DWM                                                            --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Query - Average net sales and net margin by per transaction  customer and  --
-- business unit  over one reporting period.  (Sample Measure 1)              --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)as MO_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
    ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN  
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
    join DW3_DIM_CA_PRD_RP
    on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
;


--------------------------------------------------------------------------------
-- Query - Average net sales and net margin per transaction by customer and   --
-- business unit over multiple reporting periods.                             --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS) as MO_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
    ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
    join DW3_DIM_CA_PRD_RP
    on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'
    and ID_PRD_RP between 1020 and 1024 -- Sample token ID's representing a 
                                        -- block of contiguous reporting periods
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
;
--------------------------------------------------------------------------------
-- End Average Sales Query by Customer, Business Unit for one or more         --
-- reporting periods (Sample Measure 1                                        --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Query - Average net sales and net margin by customer chain wide            --
-- over one reporting period.  (Sample Measure 2)                             --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
	,DW3_DIM_CA_PRD_RP.ID_PRD_RP
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)as MO_NT_SLS
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
	,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN  
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
	join DW3_DIM_CA_PRD_RP
	on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
	and NM_CLD = 'NRF 4-5-4 Retail Calendar'
	and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
	,DW3_DIM_CA_PRD_RP.ID_PRD_RP
;

--------------------------------------------------------------------------------
-- Query - Average net sales and net margin per transaction by customer       --
-- on a chain-wide basis over multiple reporting periods. (multi period       --
-- version for sample measure 2                                               --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
	,DW3_DIM_CA_PRD_RP.ID_PRD_RP
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS) as MO_NT_SLS
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
	,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
	,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
	join DW3_DIM_CA_PRD_RP
	on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
	and NM_CLD = 'NRF 4-5-4 Retail Calendar'
	and ID_PRD_RP between 1020 and 1024 -- Sample token ID's representing a 
	                                    -- block of contiguous reporting periods
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
	,DW3_DIM_CA_PRD_RP.ID_PRD_RP
;
--------------------------------------------------------------------------------
-- End Average Sales Query by Customer, chain wide one or more                --
-- reporting periods (Sample Measure 2)                                       --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Query - MONTHLY Average net sales and net margin per transaction by        --
-- customer and business unit for a reporting period (Sample Measure 3)       --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ----------------------------------------------------------------------------
    -- Execute function to extract calendar period as YEAR and PERIOD         --
    -- which is NRF's 4-5-4 calendar representation of a month                --
    ----------------------------------------------------------------------------
    ,dbo.FN_RP_CP_EXTR (DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
        DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
        '|',
        'PERIOD')

    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)as MO_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
    ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN  
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
    join DW3_DIM_CA_PRD_RP
    on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ,dbo.FN_RP_CP_EXTR (DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
        DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
        '|',
        'PERIOD')

-- need way to do a group by month in reporting period
;
--------------------------------------------------------------------------------
-- End QUERY MONTHLY Average Net Sales and net margin by customer and         --
-- business unit for a reporting period  (sample measure 3)                   --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Query - MONTHLY Average net sales and net margin by customer CHAIN-WIDE    -- 
-- unit for a reporting period (Sample Measure 4)                             --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ----------------------------------------------------------------------------
    -- Execute function to extract calendar period as YEAR and PERIOD         --
    -- which is NRF's 4-5-4 calendar representation of a month                --
    ----------------------------------------------------------------------------
    ,dbo.FN_RP_CP_EXTR (DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
        DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
        '|',
        'PERIOD')

    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)as MO_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
    ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN  
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
    join DW3_DIM_CA_PRD_RP
    on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    ,dbo.FN_RP_CP_EXTR (DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
        DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
        '|',
        'PERIOD')

-- need way to do a group by month in reporting period
;
--------------------------------------------------------------------------------
-- End QUERY MONTHLY Average Net Sales and net margin by customer and         --
-- business unit for a reporting period (sample measure 4)                    --
--------------------------------------------------------------------------------



-- START WITH SAMPLE MEASURE 5    
--------------------------------------------------------------------------------
-- Customer monthly average transaction count by Business Unit                --
-- Count transactions by customer by store for a reporting period and divide  --
-- that count by the number of months (NRF 4-5-4 periods) in the reporting    --
-- period.  (sample measure 5)                                                --
--------------------------------------------------------------------------------
with CT_QU_TR_QU_RP as
    (
        select
             DW3_STRD_SMRY_CT_SLSRTN.ID_CT
            ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP

            ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN_PR_RP
            ----------------------------------------------------------------------------
            -- Here we're extracting month calendar level information so we can count --
            -- it to calculate the customer-store average transaction per month for   --
            -- the reporting period.                                                  --
            ----------------------------------------------------------------------------
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
            DW3_STRD_SMRY_CT_SLSRTN
            join DW3_DIM_CA_PRD_RP
            on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
        group by
             DW3_STRD_SMRY_CT_SLSRTN.ID_CT
            ,DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP

    )
select
     CT_QU_TR_QU_RP.ID_CT            -- customer ID
    ,CT_QU_TR_QU_RP.ID_BSN_UN        -- business unit
    ,CT_QU_TR_QU_RP.ID_PRD_RP        -- reporting period
    ,CT_QU_TR_QU_RP.QU_TRN_PR_RP / CT_QU_TR_QU_RP.QU_MO_PR_RP as QU_CT_MO_AVG_TR  -- trans for month / total trans for RP
from
    CT_QU_TR_QU_RP
--------------------------------------------------------------------------------
-- End Customer Monthly Average Transaction Count by Business Unit            --
-- (sample measure 5                                                          --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Customer monthly average transaction count chain-wide                      --
-- Count transactions by customer chain wide for a reporting period and divide--
-- that count by the number of months (NRF 4-5-4 periods) in the reporting    --
-- period.  (sample measure 6)                                                --
--------------------------------------------------------------------------------
with CT_QU_TR_QU_RP as
    (
        select
             DW3_STRD_SMRY_CT_SLSRTN.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP

            ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN_PR_RP
            ----------------------------------------------------------------------------
            -- Here we're extracting month calendar level information so we can count --
            -- it to calculate the customer-store average transaction per month for   --
            -- the reporting period.                                                  --
            ----------------------------------------------------------------------------
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
            DW3_STRD_SMRY_CT_SLSRTN
            join DW3_DIM_CA_PRD_RP
            on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
        group by
             DW3_STRD_SMRY_CT_SLSRTN.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    )
select
     CT_QU_TR_QU_RP.ID_CT
    ,CT_QU_TR_QU_RP.ID_PRD_RP
    ,CT_QU_TR_QU_RP.QU_TRN_PR_RP / CT_QU_TR_QU_RP.QU_MO_PR_RP as QU_CT_MO_AVG_TR
from
    CT_QU_TR_QU_RP
;
--------------------------------------------------------------------------------
-- End Customer Monthly Average Transaction Count by Business Unit            --
-- (sample measure 6)                                                         --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Customer Average Monthly spend by TENDER type, tender media brand,         --
--  by business unit for a reporting period (sample measure 7)                --                      --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
        select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
            ,sum(MO_TRN_AMT) AS MO_TNDR_APPLD
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
         group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_BSN_UN
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,CT_TNDR_ACTVTY.TY_TND
    ,CT_TNDR_ACTVTY.CRDB_TY_CRD
    ,CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN
    ,CT_TNDR_ACTVTY.MO_TNDR_APPLD / QU_MO_PR_RP as MO_CT_BSN_UN_MO_AVG_TNDR_SPND
from
    CT_TNDR_ACTVTY
;
--------------------------------------------------------------------------------
-- END Customer Average Monthly Spend for a reporting period by Business Unit,--
-- by Tender Type, by Tender Media Brand   (sample measure 7)                 --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Customer Average Monthly spend by TENDER type, tender media brand,         --
-- by business unit for a reporting period (sample measure 8)                --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
        select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
            ,sum(MO_TRN_AMT) AS MO_TNDR_APPLD
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
         group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,CT_TNDR_ACTVTY.TY_TND
    ,CT_TNDR_ACTVTY.CRDB_TY_CRD
    ,CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN
    ,CT_TNDR_ACTVTY.MO_TNDR_APPLD / QU_MO_PR_RP as MO_CT_MO_AVG_TNDR_SPND
from
    CT_TNDR_ACTVTY
;
--------------------------------------------------------------------------------
-- END Customer Average Monthly Spend for a reporting period chain-wide,      --
-- by Tender Type, by Tender Media Brand  (sample measure 8)                  --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Customer Reporting Period Monthy Average Transaction Count by tender type  --
-- chain-wide(sample measure 9)                                               --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
       select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
            ,count(DW3_FACT_TENDER_BEHAVIOR.ID_TRN) as QU_TNDR_TRN
            ,sum(MO_TRN_AMT) AS MO_TNDR_APPLD
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
         group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,CT_TNDR_ACTVTY.TY_TND
    ,CT_TNDR_ACTVTY.CRDB_TY_CRD
    ,CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN
    ,CT_TNDR_ACTVTY.QU_TNDR_TRN / QU_MO_PR_RP
from
    CT_TNDR_ACTVTY
; 
--------------------------------------------------------------------------------
-- END Customer Reporting Period Monthy Average Transaction Count by tender   --
-- type chain-wide (sample measure 9)                                         --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Customer Reporting Period Monthy Average TRANSACTION COUNT by tender type  --
-- and Business Unit (sample measure 10)                                      --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
       select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
            ,count(DW3_FACT_TENDER_BEHAVIOR.ID_TRN) as QU_TNDR_TRN
            ,sum(MO_TRN_AMT) AS MO_TNDR_APPLD
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
         group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_BSN_UN
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,CT_TNDR_ACTVTY.TY_TND
    ,CT_TNDR_ACTVTY.CRDB_TY_CRD
    ,CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN
    ,CT_TNDR_ACTVTY.QU_TNDR_TRN / QU_MO_PR_RP
from
    CT_TNDR_ACTVTY
; 
--------------------------------------------------------------------------------
-- END Customer Reporting Period Monthy Average Transaction Count by tender   --
-- type and Business Unit (sample measure 10)                                 --
--------------------------------------------------------------------------------

-- START WITH SAMPLE MEASURE 11




--------------------------------------------------------------------------------
-- Average Tender LINE ITEM COUNT per TRANSACTION by customer and business    --
-- unit for one reporting period  (sample report 11)                          --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
       select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.ID_TRN
            ,count(DW3_FACT_TENDER_BEHAVIOR.IC_LN_ITM) as QU_LN_ITM  -- count tender line items per transaction

        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period 
         group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_FACT_TENDER_BEHAVIOR.ID_BSN_UN
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.ID_TRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_BSN_UN
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,count(CT_TNDR_ACTVTY.ID_TRN) as QU_TNDR_TRN     -- Count tender transactions
    ,sum(QU_LN_ITM)               as QU_TNDR_LN_ITM  -- Sum transaction line item counts
    ,sum(QU_LN_ITM)/count(CT_TNDR_ACTVTY.ID_TRN) as AVG_QU_TNDR_LN_ITM  -- Do the average
from
    CT_TNDR_ACTVTY
group by
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_BSN_UN
    ,CT_TNDR_ACTVTY.ID_PRD_RP
; 
--------------------------------------------------------------------------------
-- END Average Tender LINE ITEM COUNT per TRANSACTION by customer and         --
-- business unit for one reporting period  (sample meausre 11)                --
--------------------------------------------------------------------------------






--------------------------------------------------------------------------------
-- Customer percent debit/credit card (tender type and credit card type) and  --
-- tender media brand payment spent at the reatiler for a reporting period    --
-- (sample measure 12)                                                        --
--------------------------------------------------------------------------------
with CT_TNDR_ACTVTY as
    (
        select
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
            ,sum(MO_TRN_AMT) AS MO_TNDR_APPLD
            ,COUNT(dbo.FN_RP_CP_EXTR(DW3_DIM_CA_PRD_RP.CLD_PRD_HRC_LVL_NM_PTH,
                DW3_DIM_CA_PRD_RP.CLD_PRD_NM_PTH,
                '|',
                'PERIOD')) as QU_MO_PR_RP
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                        -- which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                        -- desired reporting 
      where                                             -- period
            --------------------------------------------------------------------
            -- In this sample we are limiting our analysis to non-proprietary --
            -- debit/credit cards.                                            --
            --------------------------------------------------------------------
            DW3_FACT_TENDER_BEHAVIOR.TY_TND in ('CREDIT','DEBIT')
       group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN
    )
select
     CT_TNDR_ACTVTY.ID_CT
    ,CT_TNDR_ACTVTY.ID_PRD_RP
    ,CT_TNDR_ACTVTY.TY_TND
    ,CT_TNDR_ACTVTY.CRDB_TY_CRD
    ,CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN
    ,CT_TNDR_ACTVTY.MO_TNDR_APPLD
    ,DW3_FACT_SMP_CR_DB_CRD_SPND.MO_CR_DB_TOT
    ,(CT_TNDR_ACTVTY.MO_TNDR_APPLD / DW3_FACT_SMP_CR_DB_CRD_SPND.MO_CR_DB_TOT)
         * 100 AS PE_CT_RP_SPND
from
    CT_TNDR_ACTVTY
    join DW3_FACT_SMP_CR_DB_CRD_SPND
    on CT_TNDR_ACTVTY.ID_CT = DW3_FACT_SMP_CR_DB_CRD_SPND.ID_CT
    and CT_TNDR_ACTVTY.ID_PRD_RP = DW3_FACT_SMP_CR_DB_CRD_SPND.ID_PRD_RP     
    and CT_TNDR_ACTVTY.CRDB_TY_CRD = DW3_FACT_SMP_CR_DB_CRD_SPND.TY_CRD
    and CT_TNDR_ACTVTY.CRDB_ID_TND_MD_BRN = 
        DW3_FACT_SMP_CR_DB_CRD_SPND.ID_TND_MD_BRN
;
--------------------------------------------------------------------------------
-- END Customer percent debit/credit card (tender type and credit card type)  --
-- and tender media brand payment spent at the retailer for a reporting       --
-- period (sample measure 12)                                                 --
--------------------------------------------------------------------------------




-- Comp period

--------------------------------------------------------------------------------
-- STEP 1: Create View VW_DW3_BSN_UN_QRTLY_SLS                                --
--------------------------------------------------------------------------------
-- Business Unit Quarterly Sales View - Generalized Quarter Roll up of Net    --
-- Sales and Net Margin by business unit.  Same pattern can be used for any   --
-- NRF 4-5-4 period.                                                          --
--------------------------------------------------------------------------------
-- drop view VW_DW3_BSN_UN_QRTLY_SLS;
create view VW_DW3_BSN_UN_QRTLY_SLS AS 
with ALL_PRD_NET_SLS as
    (
        Select
             DW3_STRD_SMRY_CT_SLSRTN.ID_BSN_UN
            ,DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN
            ,DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS
            ,DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN
            ,dbo.FN_REVSPLIT(DW3_DIM_CA_HIER.CLD_PRD_LVL_NM_PTH,'|') as CLD_PRD_LVL_NM_PTH
            ,dbo.FN_REVSPLIT(DW3_DIM_CA_HIER.CLD_PRD_NM_PTH,'|') as CLD_PRD_NM_PTH

        FROM
            DW3_STRD_SMRY_CT_SLSRTN
            join DW3_DIM_CA_HIER
            on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_HIER.DC_DY_BSN
            and DW3_DIM_CA_HIER.NM_CLD = 'NRF 4-5-4 Retail Calendar'
    )

select
     ALL_PRD_NET_SLS.ID_BSN_UN
    ,dbo.FN_RP_CP_EXTR
        (
             ALL_PRD_NET_SLS.CLD_PRD_LVL_NM_PTH
            ,ALL_PRD_NET_SLS.CLD_PRD_NM_PTH
            ,'|'
            ,'QUARTER'
        ) as NRF_454_YR_QTR
    ,sum(ALL_PRD_NET_SLS.MO_NT_SLS) MO_BSN_UN_QTR_NET_SLS
    ,sum(ALL_PRD_NET_SLS.MO_NET_MRGN) MO_BSN_UN_QTR_NET_MRGN
from
    ALL_PRD_NET_SLS    
group by
     ALL_PRD_NET_SLS.ID_BSN_UN
    ,dbo.FN_RP_CP_EXTR
        (
             ALL_PRD_NET_SLS.CLD_PRD_LVL_NM_PTH
            ,ALL_PRD_NET_SLS.CLD_PRD_NM_PTH
            ,'|'
            ,'QUARTER'
        ) 
;           
--------------------------------------------------------------------------------
-- End View VW_DW3_BSN_UN_QRTLY_SLS                                           --
-- END Business Unit Quarterly Sales View - Generalized Quarter Roll up of Net--
-- Sales and Net Margin by business unit.  Same pattern can be used for any   --
-- NRF 4-5-4 period.                                                          --
--------------------------------------------------------------------------------
select * into DW3_STRD_SMRY_BSN_UN_QTRLY_NET
    from VW_DW3_BSN_UN_QRTLY_SLS;
-------------------------------------------------------------------------------
-- Stored summary table to improve efficiency when doing self-joins for this --
-- year last year analysis                                                   --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- STEP 2: Sample Year to Year Comp Net Sales for 2012 by Quarter            --
-- This query uses the DW3_STRD_SMRY_BSN_UN_QTRLY_NET createdin the previous --
-- SQL code section for VW_DW3_BSN_UN_QRTLY_SLS.                             --
-------------------------------------------------------------------------------
with QTR_TO_QTR as
    (
        select
             QTR_YR.ID_BSN_UN
            ,QTR_YR.NRF_454_YR_QTR
            ,SUBSTRING(QTR_YR.NRF_454_YR_QTR,5,4) AS YR
            ,SUBSTRING(QTR_YR.NRF_454_YR_QTR,44,1) AS QTR
            ,QTR_YR.MO_BSN_UN_QTR_NET_SLS
            ,QTR_YR.MO_BSN_UN_QTR_NET_MRGN
        from
            DW3_STRD_SMRY_BSN_UN_QTRLY_NET as QTR_YR
    )		
select
     CURRENT_YR.ID_BSN_UN
    ,CURRENT_YR.NRF_454_YR_QTR
    ,CURRENT_YR.MO_BSN_UN_QTR_NET_SLS
    ,CURRENT_YR.MO_BSN_UN_QTR_NET_MRGN
    ,PRIOR_YR.NRF_454_YR_QTR
    ,PRIOR_YR.MO_BSN_UN_QTR_NET_SLS
    ,PRIOR_YR.MO_BSN_UN_QTR_NET_MRGN
from 
    QTR_TO_QTR as CURRENT_YR
    JOIN QTR_TO_QTR AS PRIOR_YR
    on CURRENT_YR.ID_BSN_UN = PRIOR_YR.ID_BSN_UN
    and PRIOR_YR.YR = CURRENT_YR.YR -1
    and CURRENT_YR.QTR = PRIOR_YR.QTR
WHERE
    CURRENT_YR.YR = 2012
--------------------------------------------------------------------------------
-- END Sample Year to Year Comp Net Sales by Quarter                          --
--------------------------------------------------------------------------------



/*

       Sample Performance Measure Queries Added based on feedback from
	   DW Work Team from meeting August 22, 2013 at NRF in Washington DC

*/	


--------------------------------------------------------------------------------
-- Sample Measure 4 - Number of transactions per time per business unit for   --
--                    a reporting period.                                     --
--                                                                            --
-- In this sample we are using Weekday as the "per time" basis for            --
-- calculating a transaction count.                                           --
--------------------------------------------------------------------------------
-- Common table expression to return a count of weekdays in a reporting       --
-- period.  This may be used as a way to calculate an average transaction     --
-- count per weekday pre business unit over a reporting period. By weekday    --
-- we mean SUNDAY through SATURDAY elements.                                  --
--------------------------------------------------------------------------------
with WK as
    (
        select
             RP.NM_PRD_RP
            ,RP.NM_CLD
            ,RP.DC_DY_BSN
            ,DATEPART(DW,RP.DC_DY_BSN) as wkdy  -- Weekday for given business 
                                                -- day date
        from
            dbo.DW3_DIM_CA_PRD_RP  RP
        where
            RP.NM_PRD_RP ='SAMPLE REPORTING PERIOD'
    )

SELECT
     SLS.ID_BSN_UN
    ,WK.NM_CLD
    ,WK.NM_PRD_RP
    ,WK.WKDY
    ,COUNT(SLS.ID_TRN)
    ,COUNT(WK.WKDY)
    -------------------------------------------------------------------------------
    -- Average Transaction Count per Weekday for a Reporting Period              --
    -- For example for a 6 month reporting period store 34 has a SUNDAY average  --
    -- transaction count of 123, a Monday average transaction count of 68.  This --
    -- is a useful way to look at staffing requirements.                         --
    -------------------------------------------------------------------------------
    ,ROUND(COUNT(SLS.ID_TRN) / COUNT(WK.WKDY),0) AS QU_TR_PER_WKDY
FROM
   -- VW_DW3_CT_SLSRTN_STRD_SMRY       SLS
    DW3_STRD_SMRY_CT_SLSRTN         SLS
    JOIN DW3_DIM_CA_PRD_RP           RP
    on SLS.DC_DY_BSN = RP.DC_DY_BSN
       and RP.NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose which calendar
       and RP.NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include desired reporting period
             
    JOIN 	                          WK
    ON WK.NM_PRD_RP = RP.NM_PRD_RP
    AND WK.NM_CLD = RP.NM_CLD
GROUP BY
     SLS.ID_BSN_UN
    ,WK.NM_CLD
    ,WK.NM_PRD_RP
    ,WK.WKDY
;
--------------------------------------------------------------------------------
-- End Sample Measure 4 Query                                                 --			  
--------------------------------------------------------------------------------   




--------------------------------------------------------------------------------
-- Sample Measure 5 - Average net sales and net margin by customer, by        --
-- channel chain wide for a reporting period                                  --
--------------------------------------------------------------------------------
-- Query - Average net sales and net margin by customer, by channel chain     --
-- chain wide over one reporting period.  (Sample Measure 2)                  --
--------------------------------------------------------------------------------
select
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_STRD_SMRY_CT_SLSRTN.ID_CHNL
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
    
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)as MO_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN) as MO_NET_MRGN
    ,COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) as QU_TRN  
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NT_SLS)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_SLS
    ,SUM(DW3_STRD_SMRY_CT_SLSRTN.MO_NET_MRGN)/COUNT(DW3_STRD_SMRY_CT_SLSRTN.ID_TRN) AS MO_AVG_NT_MRGN
from

    DW3_STRD_SMRY_CT_SLSRTN
    join DW3_DIM_CA_PRD_RP
    on DW3_STRD_SMRY_CT_SLSRTN.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
group by
     DW3_STRD_SMRY_CT_SLSRTN.ID_CT
    ,DW3_DIM_CA_PRD_RP.ID_PRD_RP
;
--------------------------------------------------------------------------------
-- End Sample Measure 5                                                       --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Sample Measure 6 - Net Sales Percent by Channel Chain wide for a reporting --
-- period                                                                     --
--------------------------------------------------------------------------------
WITH CHNL_SLS AS
    (
        SELECT
            SLS.ID_CHNL
           ,RP.ID_PRD_RP
           ,SUM(SLS.MO_NT_SLS)         AS NET_SALES
           ,SUM(SLS.MO_NET_MRGN)       AS NET_MARGIN
        FROM
            dbo.DW3_STRD_SMRY_CT_SLSRTN            SLS
            JOIN dbo.DW3_DIM_CA_PRD_RP             RP
            ON SLS.DC_DY_BSN = RP.DC_DY_BSN
            AND RP.NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
            SLS.ID_CHNL
           ,RP.ID_PRD_RP
    )
,
CHAIN_SLS AS
    (
        SELECT
            RP.ID_PRD_RP
           ,SUM(SLS.MO_NT_SLS)         AS NET_SALES
           ,SUM(SLS.MO_NET_MRGN)       AS NET_MARGIN
        FROM
            dbo.DW3_STRD_SMRY_CT_SLSRTN            SLS
            JOIN dbo.DW3_DIM_CA_PRD_RP             RP
            ON SLS.DC_DY_BSN = RP.DC_DY_BSN
            AND RP.NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
            RP.ID_PRD_RP
    )
SELECT
     CHNL_SLS.ID_CHNL
    ,CHNL_SLS.NET_SALES
    ,CHNL_SLS.NET_MARGIN
    ,CHAIN_SLS.NET_SALES
    ,CHAIN_SLS.NET_MARGIN
    ,(CHNL_SLS.NET_SALES / CHAIN_SLS.NET_SALES) * 100 AS PCT_CHNL_NET_SALES
    ,(CHNL_SLS.NET_MARGIN / CHAIN_SLS.NET_MARGIN) * 100 AS PCT_CHNL_NET_SALES
FROM
    CHNL_SLS
    JOIN CHAIN_SLS
    ON CHNL_SLS.ID_PRD_RP = CHAIN_SLS.ID_PRD_RP
--------------------------------------------------------------------------------
--  End Sample Measure 6                                                      -- 
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sample Measure 7 - Sales Percent by Tender Type by Channel, chain wide for --
-- a reporting period.  Breaks each chain-wide total tender value for a       --
-- tender type down into channels as a percentage of total RP sales           --
--------------------------------------------------------------------------------
WITH CHNL_TNDR_TYP_SMRY AS
    (
        SELECT
             SUM(MO_TNDR_SMRY.MO_TRN_ALL_TNDR_APPLD) MO_CHNL_TNDR_TYP_SMRY
            ,MO_TNDR_SMRY.ID_CHNL
            ,MO_TNDR_SMRY.TY_TND
            ,RP.ID_PRD_RP
        FROM
            DW3_STRD_SMRY_CT_TNDR     MO_TNDR_SMRY
            JOIN DW3_DIM_CA_PRD_RP    RP
            on MO_TNDR_SMRY.DC_DY_BSN = RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
             MO_TNDR_SMRY.ID_CHNL
            ,MO_TNDR_SMRY.TY_TND
            ,RP.ID_PRD_RP
    )
,
CHAIN_TNDR_SMRY AS
    (
        SELECT
             SUM(MO_TNDR_SMRY.MO_TRN_ALL_TNDR_APPLD) MO_TNDR_TYP_SMRY
        --	,MO_TNDR_SMRY.TY_TND
            ,RP.ID_PRD_RP

        FROM
            DW3_STRD_SMRY_CT_TNDR     MO_TNDR_SMRY
            JOIN DW3_DIM_CA_PRD_RP    RP
            on MO_TNDR_SMRY.DC_DY_BSN = RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
        --	 MO_TNDR_SMRY.TY_TND
            ,RP.ID_PRD_RP
    )
SELECT
     CHNL_TNDR_TYP_SMRY.ID_CHNL
    ,CHNL_TNDR_TYP_SMRY.TY_TND
    ,CHNL_TNDR_TYP_SMRY.ID_PRD_RP
    ,CHNL_TNDR_TYP_SMRY.MO_CHNL_TNDR_TYP_SMRY
    ,CHAIN_TNDR_SMRY.MO_TNDR_TYP_SMRY
    ,(CHNL_TNDR_TYP_SMRY.MO_CHNL_TNDR_TYP_SMRY / CHAIN_TNDR_SMRY.MO_TNDR_TYP_SMRY)*100 
FROM
    CHNL_TNDR_TYP_SMRY
    JOIN CHAIN_TNDR_SMRY
    ON CHNL_TNDR_TYP_SMRY.ID_PRD_RP = CHAIN_TNDR_SMRY.ID_PRD_RP
    AND CHNL_TNDR_TYP_SMRY.TY_TND = CHAIN_TNDR_SMRY.TY_TND
;
--------------------------------------------------------------------------------
-- End Sample Measure 7                                                       --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sample Measure 8 - Transaction count percent by channel Chain Wide         --
-- for a reporting period.                                                    --
--------------------------------------------------------------------------------
WITH CHNL_SLS AS
    (
        SELECT
            SLS.ID_CHNL
           ,RP.ID_PRD_RP
           ,COUNT(SLS.ID_TRN)          AS QU_TRN
        FROM
            dbo.DW3_STRD_SMRY_CT_SLSRTN            SLS
            JOIN dbo.DW3_DIM_CA_PRD_RP             RP
            ON SLS.DC_DY_BSN = RP.DC_DY_BSN
            AND RP.NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
            SLS.ID_CHNL
           ,RP.ID_PRD_RP
    )
,
CHAIN_SLS AS
    (
        SELECT
            RP.ID_PRD_RP
           ,COUNT(SLS.ID_TRN)          AS QU_TRN
        FROM
            dbo.DW3_STRD_SMRY_CT_SLSRTN            SLS
            JOIN dbo.DW3_DIM_CA_PRD_RP             RP
            ON SLS.DC_DY_BSN = RP.DC_DY_BSN
            AND RP.NM_CLD = 'NRF 4-5-4 Retail Calendar'
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
            RP.ID_PRD_RP
    )
SELECT
     CHNL_SLS.ID_CHNL
    ,CHNL_SLS.QU_TRN
    ,CHAIN_SLS.QU_TRN
    ,(CHNL_SLS.QU_TRN / CHNL_SLS.QU_TRN) * 100    AS PCT_CHNL_TRNS
FROM
    CHNL_SLS
    JOIN CHAIN_SLS
    ON CHNL_SLS.ID_PRD_RP = CHAIN_SLS.ID_PRD_RP
--------------------------------------------------------------------------------
-- End Sample Measure 8                                                       --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sample Measure 9 - Active Customer Count by REGION (in ARTS this is        --
-- represented as a BusinessUnitGroup level.  As shown below this has to be   --
-- extracted from the DW3_DIM_BSNGP table.                                    --
--------------------------------------------------------------------------------
WITH RGN AS
    (
	    ------------------------------------------------------------------------
        -- This is a temporary method for extracting a region for this sample --
		-- query.  We will address a more systemic method for extracting and  --
		-- using concatendated keys from path attributes of the dimensional   --
		-- tables in ARTS DWM in future releases.                             --
		------------------------------------------------------------------------
		select 
			 ID_BSNGP_FNC
			,ID_BSN_UN
			,NM_BSNGP_FNC
			,SUBSTRING(BSNGP_HRC_LVL_GP_NM_PTH,9,1) AS REGION
        FROM
		    DW3_DIM_BSNGP
		WHERE
		    ID_BSNGP_FNC = 1
	)
,
BSN_UN_CT as
    (
	    select DISTINCT
		     SLS.ID_BSN_UN
			,COUNT(SLS.ID_CT) AS QU_BSN_UN_CT 
		from
		    DW3_STRD_SMRY_CT_SLSRTN          SLS
			JOIN DW3_DIM_CA_PRD_RP           RP
			ON SLS.DC_DY_BSN = RP.DC_DY_BSN
			AND RP.NM_CLD = 'NRF 4-5-4 ReTail Calendar'
			and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
		     ID_BSN_UN
    		       
	)
SELECT
     RGN.REGION
	,SUM(BSN_UN_CT.QU_BSN_UN_CT) AS QU_RGN_CT

FROM
    RGN
	JOIN
	BSN_UN_CT
	ON RGN.ID_BSN_UN = BSN_UN_CT.ID_BSN_UN
GROUP BY
    RGN.REGION
;
--------------------------------------------------------------------------------
-- End Sample Measure 9                                                       --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Sample Measure 13 - Customer Acquisition Rate expressed as the percentage  --
-- ratio of newly registered customers to total active customers for a        --
-- reporting period on a CHAIN WIDE BASIS.                                    --
--------------------------------------------------------------------------------
WITH RP_NEW_CT AS
    (
       -------------------------------------------------------------------------
       -- Count customer registrations (new customers) completed for a        --
       -- reporting period based on registration date match to business day   --
       -- date for a reporting period                                         --
       -------------------------------------------------------------------------
        SELECT 
            RP.NM_PRD_RP
           ,COUNT(CT.ID_CT) QU_NW_CT
        FROM
           DW3_DIM_CT                    CT
           JOIN DW3_DIM_CA_PRD_RP        RP
           ON CT.DT_RGSTN = RP.DC_DY_BSN
           AND RP.NM_CLD = 'NRF 4-5-4 ReTail Calendar'
           AND NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
           RP.NM_PRD_RP
    )
,
CT_ACTV AS
    (
       SELECT DISTINCT
            RP.NM_PRD_RP
           ,COUNT(ID_CT) AS QU_ACTV_CT
       FROM
           DW3_STRD_SMRY_CT_SLSRTN      SLS
           JOIN DW3_DIM_CA_PRD_RP        RP
           ON SLS.DC_DY_BSN = RP.DC_DY_BSN
           AND RP.NM_CLD = 'NRF 4-5-4 ReTail Calendar'
           AND NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
       GROUP BY
           RP.NM_PRD_RP
    )

SELECT
     NEW.NM_PRD_RP
    ,NEW.QU_NW_CT
    ,ACTV.QU_ACTV_CT
    ,ROUND(NEW.QU_NW_CT / ACTV.QU_ACTV_CT,2)*100 AS PCT_RP_NW_CT
FROM
    RP_NEW_CT      NEW
    JOIN CT_ACTV   ACTV
    ON NEW.NM_PRD_RP = ACTV.NM_PRD_RP
--------------------------------------------------------------------------------
-- End Sample Measure 13                                                      --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sample Measure 16 - Customer Longevity  (IN DAYS)                          --
--------------------------------------------------------------------------------
SELECT
     CT_SLS.ID_CT     
    ,MIN(CT_SLS.DC_DY_BSN)   DC_FST_CT_SLS
    ,DATEDIFF(D,MIN(CT_SLS.DC_DY_BSN),GETDATE()) as QU_DY_CT_LONGEVITY
    ,CT.LN_PRS
    ,CT.FN_PRS
    ,CT.NM_PRS_ML
FROM
    DW3_STRD_SMRY_CT_SLSRTN   CT_SLS
    JOIN DW3_DIM_CT           CT
    ON CT_SLS.ID_CT = CT.ID_CT
GROUP BY
     CT_SLS.ID_CT
    ,CT.LN_PRS
    ,CT.FN_PRS
    ,CT.NM_PRS_ML

;
--------------------------------------------------------------------------------
-- End Sample Measure 16                                                      --
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Sample Measure 17 - Customer Profit by Reporting Period chain wide         --
--------------------------------------------------------------------------------
SELECT
     RP.NM_PRD_RP
    ,SLS.ID_CT
    ,SUM(SLS.MO_NT_SLS)          -- Net Sales
    ,SUM(SLS.MO_NET_MRGN)        -- Gross Margin on Net Sales
FROM
    DW3_STRD_SMRY_CT_SLSRTN  SLS
    JOIN DW3_DIM_CA_PRD_RP   RP
    ON SLS.DC_DY_BSN = RP.DC_DY_BSN
    AND RP.NM_CLD = 'NRF 4-5-4 ReTail Calendar'
    AND NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
GROUP BY
     RP.NM_PRD_RP
    ,SLS.ID_CT

--------------------------------------------------------------------------------
-- End Sample Measure 17                                                      --
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Sample Measure 23 - Customer use of proprietary DB/CRD as a percent of     --
-- their total use of all DB/CRD card on a CHAIN WIDE basis.  Total use as    --
-- used here means the percent of TRANSACTION COUNT.  This is intended to     --
-- determine proprietary DB/CRD penetration by customer for a reporting       --
-- period on a chain wide basis.                                              --  
--------------------------------------------------------------------------------
With CT_PRPRTY_CRDB_TNDR_ACTVTY as
    (
       -------------------------------------------------------------------------
       -- Count the number of customer transactions by specific credit        --
       -- debit credit card brand (one which is the retailer's proprietary    --
       -- brand card).                                                        --
       -------------------------------------------------------------------------                                                             
        select DISTINCT
             DW3_FACT_TENDER_BEHAVIOR.ID_CT                           -- Customer
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP                              -- Reporting period
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND                          -- Tender type
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD                     -- Credit Debit card 
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN              -- Brand
            ,COUNT(DW3_FACT_TENDER_BEHAVIOR.ID_TRN) AS QU_PRPRTY_TNDR -- Transaction
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                        -- which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                        -- desired reporting 
      where                                             -- period
            --------------------------------------------------------------------
            -- In this sample we are limiting our analysis to non-proprietary --
            -- debit/credit cards.                                            --
            --------------------------------------------------------------------
            DW3_FACT_TENDER_BEHAVIOR.TY_TND in ('CREDIT','DEBIT')
			and DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD = 'HOUSE_CARD'
            and DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN = 'SAMPLE PROP'
       group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT           -- Customer
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP              -- Reporting period
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND          -- Tender type
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_TY_CRD     -- Credit Debit card 
            ,DW3_FACT_TENDER_BEHAVIOR.CRDB_ID_TND_MD_BRN  -- Brand
    )
,
CT_ALL_CRDB_TNDR_ACTVTY as
    (
       -------------------------------------------------------------------------
       -- Count the number of customer transactions for ALL debit credit      --
       -- card transactions.  Note here we're looking at market penetration   --
       -- into DB CR card use so we only consider DB/CR tender types.  This   --
       -- query could be modified to include ALL tender types if needed.      --
       -------------------------------------------------------------------------                                                             
        select DISTINCT
             DW3_FACT_TENDER_BEHAVIOR.ID_CT           -- Customer
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP              -- Reporting period
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND          -- Tender type
            ,COUNT(DW3_FACT_TENDER_BEHAVIOR.ID_TRN) AS QU_ALL_CRDB_TNDR   -- Transaction
        from
             DW3_FACT_TENDER_BEHAVIOR
            join DW3_DIM_CA_PRD_RP
            on DW3_FACT_TENDER_BEHAVIOR.DC_DY_BSN = DW3_DIM_CA_PRD_RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                        -- which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                        -- desired reporting 
      where                                             -- period
            --------------------------------------------------------------------
            -- In this sample we are limiting our analysis to non-proprietary --
            -- debit/credit cards.                                            --
            --------------------------------------------------------------------
            DW3_FACT_TENDER_BEHAVIOR.TY_TND in ('CREDIT','DEBIT')
 
       group by
             DW3_FACT_TENDER_BEHAVIOR.ID_CT           -- Customer
            ,DW3_DIM_CA_PRD_RP.ID_PRD_RP              -- Reporting period
            ,DW3_FACT_TENDER_BEHAVIOR.TY_TND          -- Tender type
    )

SELECT
     PRPTY_CRDB.ID_CT
    ,PRPTY_CRDB.ID_PRD_RP
    ,PRPTY_CRDB.TY_TND
    ,PRPTY_CRDB.CRDB_TY_CRD                     -- Credit Debit card 
    ,PRPTY_CRDB.CRDB_ID_TND_MD_BRN              -- Brand
    ,PRPTY_CRDB.QU_PRPRTY_TNDR                  -- Prop. DB CR trans count
    ,ALL_CRDB.QU_ALL_CRDB_TNDR                  -- ALL DB CR trans count
    ,(PRPTY_CRDB.QU_PRPRTY_TNDR / ALL_CRDB.QU_ALL_CRDB_TNDR)*100  -- Prop pct of ALL
FROM
     CT_PRPRTY_CRDB_TNDR_ACTVTY       PRPTY_CRDB
     JOIN CT_ALL_CRDB_TNDR_ACTVTY     ALL_CRDB
     ON  PRPTY_CRDB.ID_CT = ALL_CRDB.ID_CT
     AND PRPTY_CRDB.ID_PRD_RP = ALL_CRDB.ID_PRD_RP
     AND PRPTY_CRDB.TY_TND = ALL_CRDB.TY_TND
;
--------------------------------------------------------------------------------
-- End Measure 23                                                             --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Measure 24 - Net sales by channel, by customer segment for a reporting     --
-- period divided by total net sales for a reporting period - this is a       --
-- chain-wide measure.  There are a lot of different ways to segment          --
-- customers in ARTS.  In this example three dimensions are identified to     --
-- segement customers - gender, marital status, life stage.  Retailers can    --
-- adapt this query to use more or fewer segmentation attributes in the       --
-- DW3_DIM_CT.                                                                --
--------------------------------------------------------------------------------
WITH SLS_BY_CT_CHNL AS
    (
       -------------------------------------------------------------------------
       -- Sum sales for each customer by channel for reporting period         --
       -------------------------------------------------------------------------
        SELECT
             SLS.ID_CT
            ,SLS.ID_CHNL
            ,RP.NM_PRD_RP
            ,SUM(SLS.MO_NT_SLS)    MO_RP_NT_SLS_CT_CHNL
            ,SUM(SLS.MO_NET_MRGN)  MO_RP_NET_MRGN_CT_CHNL

        FROM
            DW3_STRD_SMRY_CT_SLSRTN  SLS
            JOIN DW3_DIM_CA_PRD_RP     RP
            on SLS.DC_DY_BSN = RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                        -- which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                        -- desired reporting
        GROUP BY
             SLS.ID_CT
            ,SLS.ID_CHNL
            ,RP.NM_PRD_RP
    )
, RP_TOT_SLS AS
   (
       SELECT
            RP.NM_PRD_RP
           ,SUM(SLS.MO_NT_SLS)   AS MO_RP_TOT_NT_SLS
           ,SUM(SLS.MO_NET_MRGN) AS MO_RP_TOT_NET_MRGN
       FROM
           DW3_STRD_SMRY_CT_SLSRTN  SLS
            JOIN DW3_DIM_CA_PRD_RP     RP
            on SLS.DC_DY_BSN = RP.DC_DY_BSN
            and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                        -- which calendar
            and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                        -- desired reporting
       GROUP BY
           RP.NM_PRD_RP
   )

SELECT
     SLS_CT_CHNL.ID_CHNL
    ,SLS_CT_CHNL.NM_PRD_RP
    ,CT.TY_GND_PRS                -- Gender
    ,CT.CD_MRTL_STS               -- Marital status
    ,CT.CD_LF_STG                 -- Life stage
    ----------------------------------------------------------------------------
    -- Net Sales $ for customer segement and channel, total net sales and     --
    -- ratio of two to arrive at pct customer segment and channel net sales   --
    -- of total net sales for a reporting period.                             --
    ----------------------------------------------------------------------------
    ,SUM(MO_RP_NT_SLS_CT_CHNL) AS MO_RP_SLS_CHNL_CTSGMT
    ,RP_TOT_SLS.MO_RP_TOT_NT_SLS
    ,(SUM(MO_RP_NT_SLS_CT_CHNL) /RP_TOT_SLS.MO_RP_TOT_NT_SLS) * 100
    ----------------------------------------------------------------------------
    -- Gross margin on net sales for customer segement and channel, total     --
    -- gross margin on net sales and ratio of the two to arrive at pct        --
    -- customer segment and channel gross margin on net sales of total        --
    -- gross margin for a reporting period                                    --
    ----------------------------------------------------------------------------
    ,SUM(MO_RP_NET_MRGN_CT_CHNL) AS MO_RP_NET_MRGN_CHNL_CTSGMT    
    ,RP_TOT_SLS.MO_RP_TOT_NET_MRGN
    ,(SUM(MO_RP_NET_MRGN_CT_CHNL)/RP_TOT_SLS.MO_RP_TOT_NET_MRGN) * 100

FROM
    SLS_BY_CT_CHNL     SLS_CT_CHNL
    JOIN RP_TOT_SLS    RP_TOT_SLS
    ON SLS_CT_CHNL.NM_PRD_RP = RP_TOT_SLS.NM_PRD_RP

    JOIN DW3_DIM_CT    CT
    ON SLS_CT_CHNL.ID_CT = CT.ID_CT
GROUP BY
     SLS_CT_CHNL.ID_CHNL          -- Channel ID
    ,SLS_CT_CHNL.NM_PRD_RP        -- Reporting period name
    ,CT.TY_GND_PRS                -- Gender
    ,CT.CD_MRTL_STS               -- Marital status
    ,CT.CD_LF_STG                 -- Life stage
    ,RP_TOT_SLS.MO_RP_TOT_NT_SLS
    ,RP_TOT_SLS.MO_RP_TOT_NET_MRGN
;
--------------------------------------------------------------------------------
-- End Sample Measure 24                                                      --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sample Measure 26 - Average Transaction Profit per Retail transaction for  --
-- a reporting period, chain-wide                                             --
--------------------------------------------------------------------------------
SELECT
     RP.NM_PRD_RP
    ,SUM(SLS.MO_NET_MRGN)                            AS TOT_NET_MRGN
    ,COUNT(SLS.ID_TRN)                               AS TRN_COUNT
    ,ROUND(SUM(SLS.MO_NET_MRGN)/COUNT(SLS.ID_TRN),2) AS MO_RP_AVG_NET_MRGN
FROM
    DW3_STRD_SMRY_CT_SLSRTN         SLS
    jOIN DW3_DIM_CA_PRD_RP          RP
    on SLS.DC_DY_BSN = RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                -- which calendar
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                -- desired reporting
GROUP BY
    RP.NM_PRD_RP
;
--------------------------------------------------------------------------------
-- End Sample Measure 26                                                      --
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Sample Measure 27 and 28 - Average UNIT retail price (actual price paid by --
-- customers for an item chain wide for a reporting period and Average UNIT   --
-- gross margin on net sales for a reporting period                           --
--------------------------------------------------------------------------------
SELECT
     RP.NM_PRD_RP
    ,SUM(SLS.MO_NT_SLS)   AS MO_TOT_NT_SLS   -- Sum of net sales for RP
    ,SUM(SLS.MO_NET_MRGN) AS MO_TOT_NET_MRGN -- Sum of net margin for RP
    ,SUM(SLS.QU_ITM_SLD)  QU_TOT_ITM_SLD  -- Sum of item counts for transactions 
                                          -- in rpt period
   -----------------------------------------------------------------------------
   -- Calculate average UNIT net sales and gross margin                       --
   -----------------------------------------------------------------------------
    ,ROUND(SUM(SLS.MO_NT_SLS)/SUM(SLS.QU_ITM_SLD),2) AS MO_AVG_UNIT_NT_SLS
    ,ROUND(SUM(SLS.MO_NET_MRGN)/SUM(SLS.QU_ITM_SLD),2) AS MO_AVG_UNIT_NET_MRGN
      
--	,COUNT(SLS.ID_TRN)                    -- Count of retail transaction
--	,ROUND(SUM(SLS.MO_NET_MRGN)/COUNT(SLS.ID_TRN),2) AS MO_RP_AVG_NET_MRGN
FROM
    DW3_STRD_SMRY_CT_SLSRTN         SLS
    jOIN DW3_DIM_CA_PRD_RP          RP
    on SLS.DC_DY_BSN = RP.DC_DY_BSN
    and NM_CLD = 'NRF 4-5-4 Retail Calendar'    -- Filter to choose 
                                                -- which calendar
    and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'   -- Filter to include 
                                                -- desired reporting
GROUP BY
    RP.NM_PRD_RP
;
--------------------------------------------------------------------------------
-- End Sample measure 27 & 28                                                 --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sample Measure 29 - Monthly average net sale by tender type and tender     --
-- media brand for a reporting period on a chain-wide basis                   --
--------------------------------------------------------------------------------
SELECT
     RP.NM_CLD
    ,RP.NM_PRD_RP
    ,TNDR.TY_TND
    ,TNDR.CRDB_TY_CRD
    ,TNDR.CRDB_ID_TND_MD_BRN
    ,SUM(TNDR.MO_TRN_AMT) AS MO_TNDR_AMT
    ,RP_MNTH.MNTH_COUNT   AS MNTH_COUNT
    ,ROUND(SUM(TNDR.MO_TRN_AMT)/RP_MNTH.MNTH_COUNT,2) AS MO_MNTHLY_AVG_TNDR
FROM
    DW3_FACT_TENDER_BEHAVIOR   TNDR
    JOIN DW3_DIM_CA_PRD_RP     RP
    ON TNDR.DC_DY_BSN = RP.DC_DY_BSN

    JOIN VW_RP_MNTH_COUNT      RP_MNTH
    ON RP.NM_CLD = RP_MNTH.NM_CLD
    AND RP.NM_PRD_RP = RP_MNTH.NM_PRD_RP
GROUP BY
     RP.NM_CLD
    ,RP.NM_PRD_RP
    ,TNDR.TY_TND
    ,TNDR.CRDB_TY_CRD
    ,TNDR.CRDB_ID_TND_MD_BRN
    ,RP_MNTH.MNTH_COUNT
;
--------------------------------------------------------------------------------
-- End Sample Measure 29                                                      --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Sample Measure 33 - Percentage ratio of sales by channel and tender type   --
-- to total tender type sales for a reporting period on a chain wide basis.   --
-- Similar to Sample Measure 7 (same calculation)                             --
--------------------------------------------------------------------------------
-- Sample Measure 7 - Sales Percent by Tender Type by Channel, chain wide for --
-- a reporting period.  Breaks each chain-wide total tender value for a       --
-- tender type down into channels as a percentage                             --
--------------------------------------------------------------------------------
WITH CHNL_TNDR_TYP_SMRY AS
    (
		SELECT
			 SUM(MO_TNDR_SMRY.MO_TRN_ALL_TNDR_APPLD) MO_CHNL_TNDR_TYP_SMRY
			,MO_TNDR_SMRY.ID_CHNL
			,MO_TNDR_SMRY.TY_TND
			,RP.ID_PRD_RP
		FROM
			DW3_STRD_SMRY_CT_TNDR     MO_TNDR_SMRY
			JOIN DW3_DIM_CA_PRD_RP    RP
			on MO_TNDR_SMRY.DC_DY_BSN = RP.DC_DY_BSN
			and NM_CLD = 'NRF 4-5-4 Retail Calendar'
			and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
			 MO_TNDR_SMRY.ID_CHNL
			,MO_TNDR_SMRY.TY_TND
			,RP.ID_PRD_RP
    )
,
CHAIN_TNDR_SMRY AS
    (
	    SELECT
			 SUM(MO_TNDR_SMRY.MO_TRN_ALL_TNDR_APPLD) MO_TNDR_TYP_SMRY
			,MO_TNDR_SMRY.TY_TND
			,RP.ID_PRD_RP

		FROM
			DW3_STRD_SMRY_CT_TNDR     MO_TNDR_SMRY
			JOIN DW3_DIM_CA_PRD_RP    RP
			on MO_TNDR_SMRY.DC_DY_BSN = RP.DC_DY_BSN
			and NM_CLD = 'NRF 4-5-4 Retail Calendar'
			and NM_PRD_RP = 'SAMPLE REPORTING PERIOD'
        GROUP BY
			 MO_TNDR_SMRY.TY_TND
			,RP.ID_PRD_RP
	)
SELECT
     CHNL_TNDR_TYP_SMRY.ID_CHNL
	,CHNL_TNDR_TYP_SMRY.TY_TND
	,CHNL_TNDR_TYP_SMRY.ID_PRD_RP
    ,CHNL_TNDR_TYP_SMRY.MO_CHNL_TNDR_TYP_SMRY
	,CHAIN_TNDR_SMRY.MO_TNDR_TYP_SMRY
	,(CHNL_TNDR_TYP_SMRY.MO_CHNL_TNDR_TYP_SMRY / CHAIN_TNDR_SMRY.MO_TNDR_TYP_SMRY)*100 
FROM
    CHNL_TNDR_TYP_SMRY
	JOIN CHAIN_TNDR_SMRY
	ON CHNL_TNDR_TYP_SMRY.ID_PRD_RP = CHAIN_TNDR_SMRY.ID_PRD_RP
	AND CHNL_TNDR_TYP_SMRY.TY_TND = CHAIN_TNDR_SMRY.TY_TND
;
--------------------------------------------------------------------------------
-- End Sample Measure 7                                                       --
--------------------------------------------------------------------------------
