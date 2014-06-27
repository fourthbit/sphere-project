#ifdef ___LINKER_INFO
; File: "entry.m", produced by Gambit-C v4.7.2
(
407002
" entry"
((" entry"))
(
"entry"
)
(
)
(
" entry"
" entry#0"
"log"
)
(
)
(
)
 ()
)
#else
#define ___VERSION 407002
#define ___MODULE_NAME " entry"
#define ___LINKER_ID ____20_entry
#define ___MH_PROC ___H__20_entry
#define ___SCRIPT_LINE 0
#define ___SYMCOUNT 1
#define ___GLOCOUNT 3
#define ___SUPCOUNT 3
#define ___SUBCOUNT 2
#define ___LBLCOUNT 6
#define ___MODDESCR ___REF_SUB(1)
#include "gambit.h"

___NEED_SYM(___S_entry)

___NEED_GLO(___G__20_entry)
___NEED_GLO(___G__20_entry_23_0)
___NEED_GLO(___G_log)

___BEGIN_SYM
___DEF_SYM(0,___S_entry,"entry")
___END_SYM

#define ___SYM_entry ___SYM(0,___S_entry)

___BEGIN_GLO
___DEF_GLO(0," entry")
___DEF_GLO(1," entry#0")
___DEF_GLO(2,"log")
___END_GLO

#define ___GLO__20_entry ___GLO(0,___G__20_entry)
#define ___PRM__20_entry ___PRM(0,___G__20_entry)
#define ___GLO__20_entry_23_0 ___GLO(1,___G__20_entry_23_0)
#define ___PRM__20_entry_23_0 ___PRM(1,___G__20_entry_23_0)
#define ___GLO_log ___GLO(2,___G_log)
#define ___PRM_log ___PRM(2,___G_log)

___DEF_SUB_STR(___X0,18)
               ___STR8(72,101,108,108,111,32,79,98)
               ___STR8(106,45,67,32,87,111,114,108)
               ___STR2(100,33)
___DEF_SUB_VEC(___X1,5)
               ___VEC1(___REF_SYM(0,___S_entry))
               ___VEC1(___REF_PRC(1))
               ___VEC1(___REF_FIX(1))
               ___VEC1(___REF_NUL)
               ___VEC1(___REF_FAL)
               ___VEC0

___BEGIN_SUB
 ___DEF_SUB(___X0)
,___DEF_SUB(___X1)
___END_SUB



#undef ___MD_ALL
#define ___MD_ALL ___D_FP ___D_R0 ___D_R1 ___D_R4
#undef ___MR_ALL
#define ___MR_ALL ___R_FP ___R_R0 ___R_R1 ___R_R4
#undef ___MW_ALL
#define ___MW_ALL ___W_FP ___W_R0 ___W_R1 ___W_R4
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry)
___DEF_M_HLBL(___L1__20_entry)
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_entry_23_0)
___DEF_M_HLBL(___L1__20_entry_23_0)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R1 ___D_R4
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R1 ___R_R4
#undef ___PW_ALL
#define ___PW_ALL ___W_R1 ___W_R4
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry)
___DEF_P_HLBL(___L1__20_entry)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_entry)
   ___SET_GLO(2,___G_log,___PRC(4))
   ___SET_R1(___SUB(0))
   ___POLL(1)
___DEF_SLBL(1,___L1__20_entry)
   ___JUMPGLOSAFE(___SET_NARGS(1),2,___G_log)
___END_P_SW
___END_P_COD

#undef ___PH_PROC
#define ___PH_PROC ___H__20_entry_23_0
#undef ___PH_LBL0
#define ___PH_LBL0 4
#undef ___PD_ALL
#define ___PD_ALL ___D_FP ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_FP ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_FP ___W_R0
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_entry_23_0)
___DEF_P_HLBL(___L1__20_entry_23_0)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_entry_23_0)
   ___IF_NARGS_EQ(1,___NOTHING)
   ___WRONG_NARGS(0,1,0,0)
___DEF_GLBL(___L__20_entry_23_0)
   ___SET_STK(1,___R1)
   ___SET_STK(2,___R0)
   ___SET_R0(___LBL(1))
   ___ADJFP(8)
#define ___NARGS 1
___BEGIN_CFUN_VOID
#define ___ARG1 ___CFUN_ARG(1)
___BEGIN_CFUN_ARG(1,char* ___arg1)
___BEGIN_CFUN_SCMOBJ_TO_CHARSTRING(___ARG1,___arg1,1)
___BEGIN_CFUN_BODY_CLEANUP
#undef ___AT_END
printf("%s",___arg1);
#ifndef ___AT_END
#define ___AT_END
#endif
___CFUN_SET_RESULT_VOID
___END_CFUN_BODY_CLEANUP
___END_CFUN_SCMOBJ_TO_CHARSTRING(___ARG1,___arg1,1)
___END_CFUN_ARG(1)
#undef ___ARG1
___CFUN_ERROR_CLEANUP_VOID
___END_CFUN_VOID
#undef ___NARGS
   ___JUMPPRM(___NOTHING,___R0)
___DEF_SLBL(1,___L1__20_entry_23_0)
   ___ADJFP(-8)
   ___JUMPPRM(___NOTHING,___STK(2))
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_entry," entry",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_entry,0,-1)
,___DEF_LBL_RET(___H__20_entry,___IFD(___RETI,0,0,0x3fL))
,___DEF_LBL_INTRO(___H__20_entry_23_0," entry#0",___REF_FAL,2,0)
,___DEF_LBL_PROC(___H__20_entry_23_0,1,-1)
,___DEF_LBL_RET(___H__20_entry_23_0,___IFD(___RETN,2,1,0x3L))
___END_LBL

___BEGIN_MOD_PRM
___DEF_MOD_PRM(0,___G__20_entry,1)
___DEF_MOD_PRM(1,___G__20_entry_23_0,4)
___END_MOD_PRM

___BEGIN_MOD_C_INIT
___END_MOD_C_INIT

___BEGIN_MOD_GLO
___DEF_MOD_GLO(0,___G__20_entry,1)
___DEF_MOD_GLO(1,___G__20_entry_23_0,4)
___END_MOD_GLO

___BEGIN_MOD_SYM_KEY
___DEF_MOD_SYM(0,___S_entry,"entry")
___END_MOD_SYM_KEY

#endif
