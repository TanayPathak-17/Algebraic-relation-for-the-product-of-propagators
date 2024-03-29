(* ::Package:: *)

BeginPackage["AlgRel`"]


Print["AlgRel.wl v1.0\n","Author : B. Anantanarayan, Souvik Bera and Tanay Pathak"];


AlgRel::usage="The command gives the algebraic relation for the product of propagators.
AlgRed[{1,2,3,...},{k,p,m},{P,M},x,Substitution]
The form of the propagtor is assumed to be \!\(\*SubscriptBox[\(d\), \(i\)]\)=(k+\!\(\*SubscriptBox[\(p\), \(i\)]\)\!\(\*SuperscriptBox[\()\), \(2\)]\)-\!\(\*SuperscriptBox[SubscriptBox[\(m\), \(i\)], \(2\)]\), given {k,p,m}.
Value of 'i' in above can be given in the first list.
{P,M}: denotes the variables to be used in auxillary denominator.
x: denotes the variable to be used in the coefficients.
Substitutions: values to be substituted."


Begin["`Private`"]


decomposetwo[p1_,p2_,count1_,count2_,numd_,numx_,x0_,k0_,p0_,m0_,pdd0_,M0_,sub0_]:= Module[{eqns,grb1,grb2,solx,solxp,x,k,pdd,p,d,dd,m},
x=x0;
k=k0;
pdd=pdd0;
p=p0;
m=m0;
dd[i_]:= (k+pdd[i])^2-M0[i]^2;
eqns=CoefficientList[dd[numd]- (x[numx]p2 + x[numx+1]p1), k];
solxp=Solve[eqns==0,{x[numx],x[numx+1],pdd[numd]}];
solxp[[1]]=Delete[solxp[[1]],{3}];
AppendTo[solxp[[1]],Style[pdd[numd],Bold]-> (Style[Evaluate[p[count2]/.sub0],Bold])+(x[numx+1]/.Simplify[solxp[[1]]])Style[Evaluate[(p0[count1]-p0[count2])/.sub0],Bold]];
Return[Simplify[solxp[[1]]/.sub0]];
]


AlgRel[dlist_,{k0_,p0_,m0_},{pdd0_,M0_},x0_,sublist_]:= Module[{listx,decomlist,x,P,k,i,xcounter,dcounter,z,listxs,decomlistsp0,m,M,sub0,p,
decomlists,decomexp,d,dd},
ClearAll[i];
x=x0;
P=pdd0;
k=k0;p=p0;
m=m0;M=M0;
sub0=sublist;
d[i_]:= (k+p[i])^2-m[i]^2;
dd[i_]:= (k+P[i])^2-M[i]^2;
decomlist={};decomlists={};
listx= decomposetwo[d[dlist[[1]]],d[dlist[[2]]],dlist[[1]],dlist[[2]],1,1,x,k,p,m,P,M,sub0];
Table[AppendTo[decomlist,{x[i]/dd[1],dlist[[i]]}],{i,2}];
decomexp= Sum[decomlist[[i,1]]/d[decomlist[[i,2]]],{i,1,Length[decomlist]}];
dcounter=2;listxs={};

If[Length[dlist]===2,Return[{decomexp/.sub0,listx/.sub0}],
   (   
       For[i=3,i<= Length[dlist],i++,
                  Table[(   AppendTo[ listxs, decomposetwo[d[decomlist[[j,2]]],d[dlist[[i]]],decomlist[[j,2]],dlist[[i]],dcounter,2 dcounter-1,x,k,p,m,P,M,sub0]];
                                 AppendTo[decomlists,{(decomlist[[j,1]]x[2 dcounter-1])/dd[dcounter],decomlist[[j,2]]}];
                                  AppendTo[decomlists,{(decomlist[[j,1]]x[2 dcounter])/dd[dcounter],dlist[[i]]}]; dcounter=dcounter+1;
                          ),{j,1,Length[decomlist]}];

           listx=Join[listx,listxs];listxs={}; 
            decomlist={}; 
            AppendTo[decomlist,decomlists];
            decomlist= Flatten[decomlist,1]; 
            decomlists={};
            If[i==Length[dlist],decomexp= (Sum[decomlist[[i,1]] 1/d[decomlist[[i,2]]],{i,1,Length[decomlist]}]),Nothing[]];
               ]
       )];
Return[{decomexp/.sub0,Flatten[Simplify[listx/.sub0]]}];
]


End[]


EndPackage[]
