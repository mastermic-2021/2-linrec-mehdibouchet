str2ascii(s)=Vec(Vecsmall(s));
ascii2str(v)=Strchr(v);
encode(s) = {
  [ if(c==32,0,c-96) | c <- str2ascii(s), c==32 || 97<=c && c<= 122 ];
}
decode(v) = {
  ascii2str([ if(c==0,32,c+96) | c <- v]);
}

u27 = ffgen(('x^3-'x+1)*Mod(1,3),'u);
hmapcodf27= concat([0],[u27^i | i<-[0..25]]);
codf27(s) = [if(x==32,0,u27^(x-97))|x<-Vec(Vecsmall(s)),x==32||x>=97&&x<=122];
decodf27(s)= { 
  for(i=1, #s,
    c= s[i];
    s[i]= 32;
    for(j=2, #hmapcodf27, if(c == hmapcodf27[j], s[i]= j-1 + 96)
  ));
  ascii2str(s);
}


\\ **********************************************************************************
\\ Paramètres de l'exercice :                                                       *
\\                                                                                  *
\\    cipher: chiffré du message <clair> à déchiffrer                               *
\\    n: nombre d'itérations effectué par le LFSR pour chiffrer le message <clair>  *
\\    P: Polynome qui génère le LFSR en question de degré d                         *
\\    M: Matrice de transition associée au LFSR                                     *
\\    M_inv_n: Matrice inverse de M à la puissance n                                *
\\ **********************************************************************************

cipher= codf27( read("input.txt")[2] )~;
n= read("input.txt")[3];
d= 40;
P=Vecrev( -( x + u27 ), d);

M= matrix(d,d,i,j,if(j==i+1,1,0));
M[40,]= P;

\\ cipher = M^n * clair   <-----> clair= M^(-n) * cipher

M_inv_n= (M^(-1))^n;
clair= decodf27( (M_inv_n*cipher) );

print(clair);