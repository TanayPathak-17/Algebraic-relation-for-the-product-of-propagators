(* ::Package:: *)

BeginPackage["AlgRel`"]


Print["AlgRel.wl v1.0\n","Author : Tanay Pathak"];


AlgRel::usage="The command gives the numerical value of the Appell Function F2.
 FuncRed[a,b1,b2,c1,c2,x,y,precision,terms, F2show-> True]";



Begin["`Private`"]


d[i_]:= (k+p[i])^2-m[i]^2;
dd[i_]:= (k+P[i])^2-M[i]^2


decomposetwo[p1_,p2_,numd_,numx_,x0_,k0_,pdd0_]:= Module[{eqns,grb1,grb2,solx,solp,x,k,pdd},
x=x0;
k=k0;
pdd=pdd0;
eqns=CoefficientList[dd[numd]- (x[numx]p2 + x[numx+1]p1), k]; 
solxp=Solve[eqns==0,{x[numx],x[numx+1],pdd[numd]}];
Return[Simplify[solxp[[1]]]];
]


AlgRel[dlist_,{k0_,p0_,m0_},{pdd0_,M0_},x0_]:= Module[{listx,decomlist,x,P,k,xcounter,dcounter,listxs,decomlistsp0,m,M},
x=x0;P=pdd0;k=k0;p=p0;m=m0;M=M0;
d[i_]:= (k+p[i])^2-m[i]^2;
dd[i_]:= (k+P[i])^2-M[i]^2;
decomlist={};decomlists={};
listx=decomposetwo[d[dlist[[1]]],d[dlist[[2]]],1,1,x,k,P];
Table[AppendTo[decomlist,{x[i]/dd[1],dlist[[i]]}],{i,2}];
decomexp= Sum[decomlist[[i,1]]/d[decomlist[[i,2]]],{i,1,Length[decomlist]}];
dcounter=2;listxs={};

If[Length[dlist]===2,Return[{decomexp,listx}],
   (   
       For[i=3,i<= Length[dlist],i++,
                  Table[(   AppendTo[ listxs, decomposetwo[d[decomlist[[j,2]]],d[dlist[[i]]],dcounter,2 dcounter-1,x,k,P]];
                                 AppendTo[decomlists,{(decomlist[[j,1]]x[2 dcounter-1])/dd[dcounter],decomlist[[j,2]]}];
                                  AppendTo[decomlists,{(decomlist[[j,1]]x[2 dcounter])/dd[dcounter],dlist[[i]]}]; dcounter=dcounter+1;
                          ),{j,1,Length[decomlist]}];

           listx=Join[listx,listxs];listxs={}; 
            decomlist={}; 
            AppendTo[decomlist,decomlists];
            decomlist= Flatten[decomlist,1]; 
            decomlists={};
            If[i==Length[dlist],decomexp= Sum[decomlist[[i,1]] 1/d[decomlist[[i,2]]],{i,1,Length[decomlist]}],Nothing[]];
               ]
       )];
Return[{decomexp,Flatten[listx]}];
]




End[]


EndPackage[]
