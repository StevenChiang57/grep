/* Steven Chiang */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "grep.h"

const int BLKSIZE = 4096, FNSIZE = 128, LBSIZE = 4096, ESIZE = 256, GBSIZE = 256, NBRA = 5, KSIZE = 9, CCHR = 2, CEOF = 11, READ = 0, WRITE = 1;
int  peekc, lastc, given, ninbuf, io, pflag, vflag  = 1, oflag, listf, listn, tfile  = -1, tline, nleft, nbra, fchange;
unsigned nlall = 128;
unsigned int  *addr1, *addr2, *dot, *dol, *zero;
long  count;
char  Q[] = "", T[] = "TMP", savedfile[FNSIZE], file[FNSIZE], linebuf[LBSIZE], expbuf[ESIZE+4], genbuf[LBSIZE], *nextip, *linebp, *globp, *mktemp(char *), *tfname, *loc2, obuff[BLKSIZE];

int main(int argc, char *argv[]) {
	zero = (unsigned *)malloc(nlall*sizeof(unsigned));
	init();
	commands();
	quit(0);
	return 0;
}

void commands(void) {
	unsigned int *a1;
	int c;
	char lastsep;
	for (;;) {
	c = '\n';
	for (addr1 = 0;;) {
		lastsep = c;
		a1 = address();
		c = getchr();
		if (c!=',' && c!=';'){ break; }
		addr1 = a1;
	}
	if (lastsep!='\n' && a1==0){ a1 = dol; }
	if ((addr2=a1)==0) {
		given = 0;
		addr2 = dot;
	}
	else{ given = 1; }
	if (addr1==0){ addr1 = addr2; }
	switch(c) {
	case 'E':
		fchange = 0;
		c = 'e';
	case 'e':
		readfile(c);
		continue;
	case 'g':
		global(1);
		continue;
	case 'n':
		listn++;
		newline();
		print();
		continue;
	case '\n':
		if (a1==0) {
			a1 = dot+1;
			addr2 = a1;
			addr1 = a1;
		}
		if (lastsep==';')
			addr1 = a1;
		print();
		continue;
	case 'p':
	case 'P':
		newline();
		print();
		continue;
	case 'Q':
		fchange = 0;
	case 'q':
		setnoaddr();
		newline();
		quit(0);
	case EOF:
		return;
	}
	error(Q);
	}
}

void readfile(const char* c) {
	setnoaddr();
	if (vflag && fchange) {
		fchange = 0;
		error(Q);
	}
	filename(c);
	init();
	addr2 = zero;
	if ((io = open(file, 0)) < 0) {
		lastc = '\n';
		error(file);
	}
	setwide();
	squeeze(0);
	ninbuf = 0;
	c = zero != dol;
	append(getfile, addr2);
	exfile();
	fchange = c;
}

void print(void) {
	unsigned int *a1;
	nonzero();
	a1 = addr1;
	do {
		if (listn) {
			count = a1-zero;
			putd();
			putchar('\t');
		}
		puts_(getline_(*a1++));
	} while (a1 <= addr2);
	dot = addr2;
	listf = listn = pflag = 0;
}

unsigned int *address(void) {
	int sign;
	unsigned int *a, *b;
	int opcnt, nextopand;
	int c;

	nextopand = -1;
	sign = 1;
	opcnt = 0;
	a = dot;
	do {
		do c = getchr(); while (c==' ' || c=='\t');
		if ('0'<=c && c<='9') {
			peekc = c;
			if (!opcnt){ a = zero; }
			a += sign*getnum();
		} else switch (c) {
		case '/':
			compile(c);
			b = a;
			for (;;) {
				a += sign;
				if (a<=zero){ a = dol; }
				if (a>dol){ a = zero; }
				if (execute(a)){ break; }
				if (a==b){ error(Q); }
			}
			break;
		default:
			if (nextopand == opcnt) {
				a += sign;
				if (a<zero || dol<a)
					continue;       /* error(Q); */
			}
			if (c!='+' && c!='-' && c!='^') {
				peekc = c;
				if (opcnt==0){ a = 0; }
				return (a);
			}
			sign = 1;
			if (c!='+'){ sign = -sign; }
			nextopand = ++opcnt;
			continue;
		}
		sign = 1;
		opcnt++;
	} while (zero<=a && a<=dol);
	error(Q);
	return 0;
}

int getnum(void) {
	int r = 0, c;
	while ((c=getchr())>='0' && c<='9'){ r = r*10 + c - '0'; }
	peekc = c;
	return (r);
}

void setwide(void) {
	if (!given) {
		addr1 = zero + (dol>zero);
		addr2 = dol;
	}
}
void setnoaddr(void){ if (given){ error(Q);} }
void nonzero(void){ squeeze(1); }
void squeeze(int i){ if (addr1<zero+i || addr2>dol || addr1>addr2){ error(Q); } }

void newline(void) {
	int c;
	if ((c = getchr()) == '\n' || c == EOF){ return; }
	if (c=='p' || c=='l' || c=='n') {
		pflag++;
		if (c=='l'){ listf++; }
		else if (c=='n'){ listn++; }
		if ((c=getchr())=='\n'){ return; }
	}
	error(Q);
}

void filename(int comm) {
	char *p1, *p2;
	int c;
	count = 0;
	c = getchr();
	if (c=='\n' || c==EOF) {
		p1 = savedfile;
		p2 = file;
		while (*p2++ = *p1++){}
		return;
	}
	if (c!=' '){ error(Q); }
	while ((c = getchr()) == ' '){}
	if (c=='\n'){ error(Q); }
	p1 = file;
	do {
		if (p1 >= &file[sizeof(file)-1] || c==' ' || c==EOF){ error(Q); }
		*p1++ = c;
	} while ((c = getchr()) != '\n');
	*p1++ = 0;
}

void exfile(void) {
	close(io);
	io = -1;
	if (vflag) {
		putd();
		putchar('\n');
	}
}

void error(char *s) {
	int c;
	listf = listn = 0;
	putchar('?');
	puts_(s);
	count = 0;
	lseek(0, (long)0, 2);
	pflag = 0;
	if (globp)
		lastc = '\n';
	globp = 0;
	peekc = lastc;
	if(lastc)
		while ((c = getchr()) != '\n' && c != EOF) {}
	if (io > 0) {
		close(io);
		io = -1;
	}
}

int getchr(void) {
	char c;
	if (lastc=peekc) {
		peekc = 0;
		return(lastc);
	}
	if (globp) {
		if ((lastc = *globp++) != 0){ return(lastc); }
		globp = 0;
		return(EOF);
	}
	if (read(0, &c, 1) <= 0){ return(lastc = EOF); }
	lastc = c&0177;
	return(lastc);
}

int getfile(void) {
	int c;
	char *lp, *fp;

	lp = linebuf;
	fp = nextip;
	do {
		if (--ninbuf < 0) {
			if ((ninbuf = read(io, genbuf, LBSIZE)-1) < 0){ return(EOF); }
			fp = genbuf;
			fp = genbuf;
		}
		c = *fp++;
		*lp++ = c;
		count++;
	} while (c != '\n');
	*--lp = 0;
	nextip = fp;
	return(0);
}

void putfile(void) {
	unsigned int *a1;
	int n;
	char *fp, *lp;
	int nib;
	nib = BLKSIZE;
	fp = genbuf;
	a1 = addr1;
	do { lp = getline_(*a1++); } while (a1 <= addr2);
	n = fp-genbuf;
}

int append(int (*f)(void), unsigned int *a){
	unsigned int *a1, *a2, *rdot;
	int nline = 0, tl;
	dot = a;
	while ((*f)() == 0) {
		tl = putline();
		nline++;
		a1 = ++dol;
		a2 = a1+1;
		rdot = ++dot;
		while (a1 > rdot){ *--a2 = *--a1; }
		*rdot = tl;
	}
	return(nline);
}

void quit(int n){
	if (vflag && fchange && dol!=zero){
		fchange = 0;
		error(Q);
	}
	unlink(tfname);
	exit(0);
}

char *getline_(unsigned int tl){
	char *bp, *lp;
	int nl;
	lp = linebuf;
	bp = getblock(tl, READ);
	nl = nleft;
	tl &= ~((BLKSIZE/2)-1);
	while (*lp++ = *bp++)
		if (--nl == 0) {
			bp = getblock(tl+=(BLKSIZE/2), READ);
			nl = nleft;
		}
	return(linebuf);
}

int putline(void){
	char *bp, *lp;
	int nl;
	unsigned int tl;
	fchange = 1;
	lp = linebuf;
	tl = tline;
	bp = getblock(tl, WRITE);
	nl = nleft;
	tl &= ~((BLKSIZE/2)-1);
	while (*bp = *lp++) {
		if (*bp++ == '\n') {
			*--bp = 0;
			linebp = lp;
			break;
		}
	}
	nl = tline;
	tline += (((lp-linebuf)+03)>>1)&077776;
	return(nl);
}

char *getblock(unsigned int atl, int iof){
	int bno, off;
	bno = (atl/(BLKSIZE/2));
	off = (atl<<1) & (BLKSIZE-1) & ~03;
	nleft = BLKSIZE - off;
	return(obuff+off);
}

void init(void){
	close(tfile);
	tline = 2;
	close(creat(tfname, 0600));
	tfile = open(tfname, 2);
	dot = dol = zero;
}

void global(int k){
	char *gp;
	int c;
	unsigned int *a1;
	char globuf[GBSIZE];
	if (globp){ error(Q); }
	setwide();
	squeeze(dol>zero);
	if ((c=getchr())=='\n'){ error(Q); }
	compile(c);
	gp = globuf;
	while ((c = getchr()) != '\n') {
		if (c==EOF){ error(Q); }
		*gp++ = c;
		if (gp >= &globuf[GBSIZE-2]){ error(Q); }
	}
	if (gp == globuf){ *gp++ = 'p'; }
	*gp++ = '\n';
	*gp++ = 0;
	for (a1=zero; a1<=dol; a1++) {
		*a1 &= ~01;
		if (a1>=addr1 && a1<=addr2 && execute(a1)==k){ *a1 |= 01; }
	}
	for (a1=zero; a1<=dol; a1++) {
		if (*a1 & 01) {
			*a1 &= ~01;
			dot = a1;
			globp = globuf;
			commands();
			a1 = zero;
		}
	}
}

void compile(int eof){
	int c;
	char *ep = expbuf;
	char *lastep;
	if ((c = getchr()) == '\n') {
		peekc = c;
		c = eof;
	}
	nbra = 0;
	peekc = c;
	lastep = 0;
	for (;;) {
		c = getchr();
		if (c == '\n') {
			peekc = c;
			c = eof;
		}
		if (c==eof) {
			*ep++ = CEOF;
			return;
		}
		if (c!='*'){ lastep = ep; }
		switch (c) {
		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
}

int execute(unsigned int *addr){
	char *p1, *p2;
	int c;
	p2 = expbuf;
	p1 = getline_(*addr);
	if (*p2==CCHR) {
		c = p2[1];
		do {
			if (*p1!=c) {continue;}
			if (advance(p1, p2)) {
				return(1);
			}
		} while (*p1++);
		return(0);
	}
	return(0);
}

int advance(char *lp, char *ep) {
	for (;;) switch (*ep++) {
	case CCHR: if (*ep++ == *lp++){continue;} return(0);
	case CEOF: loc2 = lp; return(1);
	default: error(Q);
	}
}

void putd(void){
	int r = count%10;
	count /= 10;
	if (count){ putd(); }
	putchar(r + '0');
}

void puts_(char *sp){
	while (*sp){putchar(*sp++); }
	putchar('\n');
}
