(* ::Package:: *)

BeginPackage["MSCB2d`"]


(* ::Section:: *)
(*Package information*)


Print[
"MSCB2d is a Mathematica package that evaluates
Momentum-Space Conformal Blocks in a 2d CFT.

The Wightman 4-point function
  \[LeftAngleBracket]\[CapitalOmega]|\!\(\*SubscriptBox[\(O\), \(4\)]\)(\!\(\*SubscriptBox[\(p\), \(4\)]\))\!\(\*SubscriptBox[\(O\), \(3\)]\)(\!\(\*SubscriptBox[\(p\), \(3\)]\))\!\(\*SubscriptBox[\(O\), \(2\)]\)(\!\(\*SubscriptBox[\(p\), \(2\)]\))\!\(\*SubscriptBox[\(O\), \(1\)]\)(\!\(\*SubscriptBox[\(p\), \(1\)]\))|\[CapitalOmega]\[RightAngleBracket]
    = (2\[Pi]\!\(\*SuperscriptBox[\()\), \(2\)]\) \[Delta](\!\(\*SubscriptBox[\(p\), \(1\)]\)+\!\(\*SubscriptBox[\(p\), \(2\)]\)+\!\(\*SubscriptBox[\(p\), \(3\)]\)+\!\(\*SubscriptBox[\(p\), \(4\)]\)) G(\!\(\*SubscriptBox[\(p\), \(a\)]\))
admits the conformal block expansion
  G(\!\(\*SubscriptBox[\(p\), \(a\)]\))=\!\(\*UnderscriptBox[\(\[Sum]\), \(x\)]\) \!\(\*SubscriptBox[\(\[Lambda]\), \(12  x\)]\)\!\(\*SubscriptBox[\(\[Lambda]\), \(x34\)]\) \!\(\*SubscriptBox[\(G\), \(x\)]\)(\!\(\*SubscriptBox[\(p\), \(a\)]\))
This package provides explicit expressions for \!\(\*SubscriptBox[\(G\), \(x\)]\)
as a function of the conformal weights (\!\(\*SubscriptBox[\(h\), \(a\)]\),\!\(\*SubscriptBox[OverscriptBox[\(h\), \(_\)], \(a\)]\)) of
the external (1,2,3,4) and exchange (x) operators
and of the momenta \!\(\*SubscriptBox[\(p\), \(a\)]\).

The following conventions are used for the momenta:
  \!\(\*SubscriptBox[\(p\), \(i\)]\)=\!\(\*SubscriptBox[\(p\), \(1\)]\)   
  \!\(\*SubscriptBox[\(p\), \(f\)]\)=-\!\(\*SubscriptBox[\(p\), \(4\)]\)
  k=\!\(\*SubscriptBox[\(p\), \(1\)]\)+\!\(\*SubscriptBox[\(p\), \(2\)]\)=-\!\(\*SubscriptBox[\(p\), \(3\)]\)-\!\(\*SubscriptBox[\(p\), \(4\)]\)
The 4-point function is identically zero unless
all 3 momenta \!\(\*SubscriptBox[\(p\), \(i\)]\), \!\(\*SubscriptBox[\(p\), \(f\)]\) and k lie in the future
light-cone."
]


(* ::Section:: *)
(*Definitions*)


HolomorphicCB::usage="HolomorphicCB[\!\(\*SubscriptBox[\(h\), \(4\)]\),\!\(\*SubscriptBox[\(h\), \(3\)]\),\!\(\*SubscriptBox[\(h\), \(2\)]\),\!\(\*SubscriptBox[\(h\), \(1\)]\),\!\(\*SubscriptBox[\(h\), \(x\)]\)][\!\(\*SubsuperscriptBox[\(p\), \(f\), \(+\)]\),\!\(\*SuperscriptBox[\(k\), \(+\)]\),\!\(\*SubsuperscriptBox[\(p\), \(i\), \(+\)]\)]

The 2d conformal block can be split into left-
and right-moving parts (or holomorphic blocks)
  \!\(\*SubscriptBox[\(G\), \(x\)]\)(\!\(\*SubscriptBox[\(p\), \(a\)]\))=\!\(\*SubsuperscriptBox[\(G\), \(x\), \(+\)]\)(\!\(\*SubsuperscriptBox[\(p\), \(a\), \(+\)]\))\!\(\*SubsuperscriptBox[\(G\), \(x\), \(-\)]\)(\!\(\*SubsuperscriptBox[\(p\), \(a\), \(-\)]\))
This function computes \!\(\*SubsuperscriptBox[\(G\), \(x\), \(+\)]\)(\!\(\*SubsuperscriptBox[\(p\), \(a\), \(+\)]\)), which depends on
the components \!\(\*SubsuperscriptBox[\(p\), \(f\), \(+\)]\), \!\(\*SuperscriptBox[\(k\), \(+\)]\) and \!\(\*SubsuperscriptBox[\(p\), \(i\), \(+\)]\) of the momenta, as
well as on the (holomorphic) conformal weights
\!\(\*SubscriptBox[\(h\), \(4\)]\), \!\(\*SubscriptBox[\(h\), \(3\)]\), \!\(\*SubscriptBox[\(h\), \(2\)]\), \!\(\*SubscriptBox[\(h\), \(1\)]\) and \!\(\*SubscriptBox[\(h\), \(x\)]\).

When all external operators are identical, it
is possible to use instead
  HolomorphicCB[h,\!\(\*SubscriptBox[\(h\), \(x\)]\)][\!\(\*SubsuperscriptBox[\(p\), \(f\), \(+\)]\),\!\(\*SuperscriptBox[\(k\), \(+\)]\),\!\(\*SubsuperscriptBox[\(p\), \(i\), \(+\)]\)]
"


GlobalCB::usage="GlobalCB[\!\(\*SubscriptBox[\(O\), \(4\)]\),\!\(\*SubscriptBox[\(O\), \(3\)]\),\!\(\*SubscriptBox[\(O\), \(2\)]\),\!\(\*SubscriptBox[\(O\), \(1\)]\),\!\(\*SubscriptBox[\(O\), \(x\)]\)][\!\(\*SubscriptBox[\(p\), \(f\)]\),k,\!\(\*SubscriptBox[\(p\), \(i\)]\)]
- \!\(\*SubscriptBox[\(O\), \(a\)]\) are operators defined either in terms of
  conformal weights with the command Operator[h,\!\(\*OverscriptBox[\(h\), \(_\)]\)]
  or in terms of scaling dimension and spin with
  the command Operator\[CapitalDelta]j[\[CapitalDelta],j]
- \!\(\*SubscriptBox[\(p\), \(f\)]\), k and \!\(\*SubscriptBox[\(p\), \(i\)]\) are momenta defined either in
  ordinary coordinates with the command
  Momentum[\!\(\*SuperscriptBox[\(p\), \(0\)]\),\!\(\*SuperscriptBox[\(p\), \(1\)]\)] or in light-cone coordinates
  with the command MomentumLC[\!\(\*SuperscriptBox[\(p\), \(+\)]\),\!\(\*SuperscriptBox[\(p\), \(-\)]\)]

When all external operators are identical, it
is possible to use
  GlobalCB[O,\!\(\*SubscriptBox[\(O\), \(x\)]\)][\!\(\*SubscriptBox[\(p\), \(f\)]\),k,\!\(\*SubscriptBox[\(p\), \(i\)]\)]"


Operator::usage="Operator[h,\!\(\*OverscriptBox[\(h\), \(_\)]\)]
defines an operator with conformal weights
(h,\!\(\*OverscriptBox[\(h\), \(_\)]\))"


Operator\[CapitalDelta]j::usage="Operator\[CapitalDelta]j[\[CapitalDelta],j]
defines an operator with scaling dimension \[CapitalDelta]
and spin j

Note that when using this definition one
should remember to take into accounts operators
with both positive and negative spin j"


Momentum::usage="Momentum[\!\(\*SuperscriptBox[\(p\), \(0\)]\),\!\(\*SuperscriptBox[\(p\), \(1\)]\)]
defines a 2-momentum in terms of its components
(\!\(\*SuperscriptBox[\(p\), \(0\)]\),\!\(\*SuperscriptBox[\(p\), \(1\)]\))"


MomentumLC::usage="MomentumLC[\!\(\*SuperscriptBox[\(p\), \(+\)]\),\!\(\*SuperscriptBox[\(p\), \(-\)]\)]
defines a 2-momentum in terms of its light-cone
coordinates (\!\(\*SuperscriptBox[\(p\), \(+\)]\),\!\(\*SuperscriptBox[\(p\), \(-\)]\))=(\!\(\*SuperscriptBox[\(p\), \(0\)]\)-\!\(\*SuperscriptBox[\(p\), \(1\)]\),\!\(\*SuperscriptBox[\(p\), \(0\)]\)+\!\(\*SuperscriptBox[\(p\), \(1\)]\))"


(* ::Section:: *)
(*Package core*)


Begin["`Private`"]


(* ::Subsection:: *)
(*3-point function*)


V[hf_,h0_,hi_][kf_,ki_]:=((2\[Pi])^2
Piecewise[{
	{(-kf)^(2hf-1) ki^(h0+hi-hf-1) Hypergeometric2F1[1-h0-hi+hf,hi+hf-h0,2hf,-kf/ki]/(Gamma[h0+hi-hf]Gamma[2hf]),ki>0&&kf<0&&ki+kf>0},
	{(-kf)^(h0-hi+hf-1) ki^(2hi-1)  Hypergeometric2F1[1-h0-hf+hi,hi+hf-h0,2hi,-ki/kf]/(Gamma[h0+hf-hi]Gamma[2hi]),ki>0&&kf<0&&ki+kf<0}
},0])


(* ::Subsection:: *)
(*Function HolomorphicCB*)


HolomorphicCB::ZeroConformalWeight="The holomorphic conformal block is ill-defined: the exchange operator has conformal weight 0 but the external operators have distinct conformal weights. This cannot happen in a healthy CFT. One must have `1`=`2` and `3`=`4`"


HolomorphicCB[h4_,h3_,h2_,h1_,hx_][kf_,k_,ki_]:=(
If[NumericQ[hx]&&hx==0,
	If[h1===h2&&h3===h4,
		0,
		Message[HolomorphicCB::ZeroConformalWeight,h4,h3,h2,h1]
	],
	Gamma[2hx]k^(1-2hx) V[h4,h3,hx][kf,k]V[hx,h2,h1][-k,ki]/(Pi Sqrt[2])
])


HolomorphicCB[h_,hx_][pf_,k_,pi_]:=HolomorphicCB[h,h,h,h,hx][pf,k,pi]


(* ::Subsection:: *)
(*Operators*)


Operator/:MakeBoxes[Operator[h_,hbar_],StandardForm]:=SubscriptBox["O",RowBox[{"(",ToBoxes[h],",",ToBoxes[hbar],")"}]]


Operator\[CapitalDelta]j[\[CapitalDelta]_,j_]:=Operator[(\[CapitalDelta]+j)/2,(\[CapitalDelta]-j)/2]


(* ::Subsection:: *)
(*Momenta*)


(*MomentumLC/:MakeBoxes[MomentumLC[pp_,pm_],StandardForm]:=Module[{p0,p1},
	{p0,p1}={(pp+pm)/2,(pp-pm)/2};
	RowBox[{"(",ToBoxes[p0,StandardForm],",",ToBoxes[p1,StandardForm],")"}]
]*)


MomentumLC/:MakeBoxes[MomentumLC[pp_,pm_],StandardForm]:=RowBox[{"(",ToBoxes[(pp+pm)/2],",",ToBoxes[(pp-pm)/2],")"}]


Momentum[p0_,p1_]:=MomentumLC[p0+p1,p0-p1]


MomentumLC/:Times[c_,MomentumLC[pp_,pm_]]:=MomentumLC[c*pp,c*pm]/;NumericQ[c]


MomentumLC/:Plus[MomentumLC[p1p_,p1m_],MomentumLC[p2p_,p2m_]]:=MomentumLC[p1p+p2p,p1m+p2m]


(* ::Subsection:: *)
(*Function GlobalCB*)


GlobalCB[O4_Operator,O3_Operator,O2_Operator,O1_Operator,Ox_Operator][pf_MomentumLC,k_MomentumLC,pi_MomentumLC]:=HolomorphicCB[O4[[1]],O3[[1]],O2[[1]],O1[[1]],Ox[[1]]][pf[[1]],k[[1]],pi[[1]]]HolomorphicCB[O4[[2]],O3[[2]],O2[[2]],O1[[2]],Ox[[2]]][pf[[2]],k[[2]],pi[[2]]]


GlobalCB[OO_Operator,Ox_Operator][pf_MomentumLC,k_MomentumLC,pi_MomentumLC]:=GlobalCB[OO,OO,OO,OO,Ox][pf,k,pi]


End[]


EndPackage[]
