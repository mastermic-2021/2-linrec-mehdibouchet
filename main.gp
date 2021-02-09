u27 = ffgen(('x^3-'x+1)*Mod(1,3),'u);
P(u)= u^3 - u + 1;

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

str2ascii(s)=Vec(Vecsmall(s));
ascii2str(v)=Strchr(v);
encode(s) = {
  [ if(c==32,0,c-96) | c <- str2ascii(s), c==32 || 97<=c && c<= 122 ];
}
decode(v) = {
  ascii2str([ if(c==0,32,c+96) | c <- v]);
}




linrec(u0,c,n) = {
  if(type(c)=="t_POL",c=Vecrev(c));
  d = #c - 1;
  if(type(u0)=="t_POL",u0=Vecrev(u,d));
  v = Vec(u0,n);
  for(k=d+1,n, v[k] = -sum(i=0,d-1,v[k-d+i]*c[i+1]));
  v;
}

annul_lin(u,d) = {
  my(m=matrix(d,d));
  for(i=1,d,for(j=1,i,
    m[j,i-j+1]=u[i];
    m[d-j+1,d-i+j] = u[2*d-i];
  ));
  Polrev(matsolve(m,-Col(u[d+1..d+d])))+x^d;
}

sys_lin(u,d) = {
  my(m=matrix(d,d));
  for(i=1,d,for(j=1,i,
    m[j,i-j+1]=u[i];
    m[d-j+1,d-i+j] = u[2*d-i];
  ));m;
}

cipher1= codf27( read("input.txt")[2] );
cipher2= codf27( read("input.txt")[1] );
n= read("input.txt")[3];

cipher= concat(cipher1, cipher2);
\\ m(x)= annul_lin(cipher, 40);
\\ print(m(x));
m(x)= x^40 + x + u27;

A(x)= Mod(x, m(x))^n;
reverseA(x)= lift( modreverse(A(x)) );
print(reverseA(x));
clair= linrec(cipher1, reverseA(x), 40);
print( decodf27(cipher1+clair) );