import os,json,urllib.request as R
S=1;C=0;Y=-1;N=0;L=200000;F='abcdefgh'
V=[0,100,320,330,500,900,0]
KN=[-33,-31,-18,-14,14,18,31,33];D=[-16,-1,1,16,-17,-15,15,17];KI=sorted(D)
def Z(q,v):
 t=abs(v);r,c=q>>4,q&7;a=r if v>0 else 7-r
 return(V[t]+(14-abs(2*c-7)-abs(2*r-7))*(1<t<6)+6*a*(t==1))*((v>0)*2-1)
def P(f):
 global B,S,C,Y,KP,SC
 B=[0]*128;S=1;C=0;Y=-1
 p=f.split();r=7;c=0
 for ch in p[0]:
  if ch=='/':r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:B[r*16+c]=('pnbrqk'.index(ch.lower())+1)*((ch<'a')*2-1);c+=1
 S=1-2*(p[1]=='b');C=sum(1<<'KQkq'.index(ch)for ch in p[2]if ch in'KQkq')
 Y=(int(p[3][1])-1)*16+F.index(p[3][0])if p[3]!='-'else-1
 KP=[B.index(6),B.index(-6)];SC=sum(map(Z,range(128),B))
def A(q,by):
 for O,t,m in(((15,17),1,-by),(KN,2,1),(KI,6,1)):
  for d in O:
   s=q+m*d
   if not s&0x88 and B[s]==by*t:return 1
 for i,d in enumerate(D):
  s=q
  while not(s:=s+d)&0x88:
   v=B[s]
   if v:
    if v in(by*5,by*(3+(i<4))):return 1
    break
 return 0
def G():
 ms=[];pr=7 if S>0 else 0;dd=16*S
 for f in range(128):
  v=B[f]
  if f&0x88 or v*S<=0:continue
  t=abs(v)
  if t==1:
   o=f+dd
   if not B[o]:
    if o>>4==pr:ms+=[(f,o,p)for p in(5,4,3,2)]
    else:
     ms.append((f,o,0))
     if f>>4==1+5*(S<0)and not B[o+dd]:ms.append((f,o+dd,0))
   for o in(f+dd-1,f+dd+1):
    if o&0x88:continue
    if B[o]*S<0:ms+=[(f,o,p)for p in((5,4,3,2)if o>>4==pr else(0,))]
    elif o==Y:ms.append((f,o,9))
  else:
   ds=(KN,D[4:],D[:4],D,KI)[t-2]
   for d in ds:
    o=f+d
    while not o&0x88:
     if B[o]:
      if B[o]*S<0:ms.append((f,o,0))
      break
     ms.append((f,o,0))
     if t in(2,6):break
     o+=d
   if t==6:
    h=116-112*(S>0);e=-S;z=2*(S<0)
    for k,bt in((1,1<<z),(-1,2<<z)):
     if C&bt and B[h+k]==B[h+2*k]==0 and(k>0 or B[h+3*k]==0)and not A(h,e)and not A(h+k,e)and not A(h+2*k,e):ms.append((h,h+2*k,0))
 return ms
def M(m,u=0):
 global S,C,Y,SC
 f,t,p=m
 if u:
  S=-S;mi=0 if S>0 else 1;cap,C,Y,KP[mi],SC=u
  B[f]=S if 1<p<9 else B[t]
  if p==9:B[t-16*S]=cap
  B[t]=cap*(p!=9)
  if B[f]==6*S and abs(t-f)==2:rf,rt=(t+1,t-1)if t>f else(t-2,t+1);B[rf]=B[rt];B[rt]=0
  return
 mi=0 if S>0 else 1;cap=B[t-16*S*(p==9)];un=(cap,C,Y,KP[mi],SC)
 if p==9:B[t-16*S]=0
 pc=B[f];SC-=Z(f,pc)+Z(t-16*S*(p==9),cap)
 B[f]=0;B[t]=S*p if 1<p<9 else pc;SC+=Z(t,B[t])
 if pc==6*S:
  KP[mi]=t
  if abs(t-f)==2:rf,rt=(t+1,t-1)if t>f else(t-2,t+1);SC-=Z(rf,B[rf]);B[rt]=B[rf];B[rf]=0;SC+=Z(rt,B[rt])
  C&=(-13,-4)[S>0]
 for z in(f,t):C&=~{0:2,7:1,112:8,119:4}.get(z,0)
 Y=(f+t)//2 if pc==S and abs(t-f)==32 else-1
 S=-S
 return un
def T(m):
 u=M(m);return u if not A(KP[S>0],S)else(M(m,u)or 0)
def E():return SC*S
def Q(a,b,q):
 global N
 N+=1;bs=E()
 if N>L or bs>=b or q<=0:return bs
 a=max(a,bs)
 for m in G():
  v=B[m[1]]
  if not v and m[2]!=9:continue
  if bs+V[abs(v)]+150<a:continue
  un=T(m)
  if not un:continue
  sc=-Q(-b,-a,q-1);M(m,un)
  bs=max(bs,sc);a=max(a,bs)
  if a>=b:break
 return bs
def Sr(d,a,b):
 global N
 N+=1
 if N>L:return E()
 if d<=0:return Q(a,b,6)
 ms=G();ms.sort(key=lambda m:(-V[abs(B[m[1]])],V[abs(B[m[0]])]));lg=0;bs=-99999
 for m in ms:
  un=T(m)
  if not un:continue
  lg+=1;sc=-Sr(d-1,-b,-a);M(m,un)
  a=max(a,bs:=max(bs,sc))
  if a>=b:break
 if not lg:return -99999 if A(KP[S<0],-S)else 0
 return bs
def W(m):a,b,p=m;return F[a&7]+str((a>>4)+1)+F[b&7]+str((b>>4)+1)+('nbrq'[p-2]if 1<p<9 else'')
def Ch(fen,legal):
 global N
 P(fen);cs=[]
 for m in G():
  un=W(m)in legal and T(m)
  if un:cs.append(m);M(m,un)
 if not cs:return legal[0]
 bs=cs[0];N=0
 for d in range(9):
  bb=bs;ba=-99999;cs.sort(key=lambda m:m!=bb)
  for m in cs:
   un=M(m);sc=-Sr(d,-99999,99999);M(m,un)
   if sc>ba:ba,bb=sc,m
   if N>L:break
  if N>L:break
  bs=bb
 return W(bs)
def main():
 U=os.getenv('MAKEFUN_ENV_URL')
 def rq(p,o):
  q=R.Request(U+p,data=json.dumps(o).encode());return json.load(R.urlopen(q))
 st=rq('/game',{})
 while 1:
  s=st.get('status')
  if s=='done':break
  if s!='your_move':st=rq('/game',{});continue
  mv=Ch(st['fen'],st['legal_moves']);st=rq('/move',{'game':st['game'],'move':mv})
if __name__=='__main__':main()
