#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char *json_buf = NULL;
static char *jp = NULL;

static void jskip_ws(void){ while(*jp && isspace((unsigned char)*jp)) jp++; }

static int jparse_str(char *out, int maxlen){
    jskip_ws();
    if(*jp != '"') return 0;
    jp++;
    int i=0;
    while(*jp && *jp != '"'){
        if(*jp == '\\'){
            jp++;
            if(*jp == 'u'){
                if(i<maxlen-1) out[i++] = '\\';
                if(i<maxlen-1) out[i++] = 'u';
                jp++;
                for(int k=0;k<4 && *jp;k++){
                    if(i<maxlen-1) out[i++] = *jp;
                    jp++;
                }
                continue;
            }
            if(i<maxlen-1) out[i++] = *jp;
        } else {
            if(i<maxlen-1) out[i++] = *jp;
        }
        jp++;
    }
    out[i]=0;
    if(*jp == '"') jp++;
    return 1;
}

static int jparse_int(void){
    jskip_ws();
    int neg=0, val=0;
    if(*jp=='-'){ neg=1; jp++; }
    while(*jp && isdigit((unsigned char)*jp)){ val=val*10+(*jp-'0'); jp++; }
    return neg ? -val : val;
}

static void jskip_val(void){
    jskip_ws();
    if(*jp == '"'){ char tmp[512]; jparse_str(tmp,512); }
    else if(*jp == '{'){
        jp++; int d=1;
        while(*jp && d>0){ if(*jp=='{')d++; else if(*jp=='}')d--; jp++; }
    }
    else if(*jp == '['){
        jp++; int d=1;
        while(*jp && d>0){ if(*jp=='[')d++; else if(*jp==']')d--; jp++; }
    }
    else if(strncmp(jp,"null",4)==0) jp+=4;
    else if(strncmp(jp,"true",4)==0) jp+=4;
    else if(strncmp(jp,"false",5)==0) jp+=5;
    else { while(*jp && *jp!=',' && *jp!='}' && *jp!=']') jp++; }
}

static int jfind_key(const char *key){

    while(*jp && *jp != '}'){
        jskip_ws();
        if(*jp != '"') { jp++; continue; }
        char k[128]; jparse_str(k,128);
        jskip_ws();
        if(*jp == ':') jp++;
        if(strcmp(k,key)==0) return 1;
        jskip_val();
        jskip_ws();
        if(*jp == ',') jp++;
    }
    return 0;
}

static char* jload_file(const char *path){
    FILE *f = fopen(path,"rb");
    if(!f){ f = fopen("data.json","rb"); }
    if(!f){ fprintf(stderr,"Cannot open JSON file: %s\n",path); return NULL; }
    fseek(f,0,SEEK_END); long sz=ftell(f); fseek(f,0,SEEK_SET);
    char *buf = (char*)malloc(sz+1);
    if(!buf){ fclose(f); return NULL; }
    fread(buf,1,sz,f); buf[sz]=0; fclose(f);
    return buf;
}

#define MAXC 64
#define MAXPID 32
#define MAXN 64
#define MAXB 32
#define MAXCAT 32
#define MAXCUR 8
#define MAXWH 8
#define MAXISBN 32
#define MAXDESC 128
#define MAXEXT 16
#define MAXCOUNTRIES 64
#define MAXCITIES 512
#define MAXPRODS 8192
#define SENTINEL -1L
#define DATFILE "records.dat"
#define HEAP_SZ 5
#define JSONFILE "Assignment -2.json"

#pragma pack(push,1)
typedef struct {
    char pid[MAXPID]; char country[MAXC]; char ccode[8]; char city[MAXC];
    char name[MAXN]; char brand[MAXB]; char category[MAXCAT];
    int price; char currency[MAXCUR]; int stock;
    char warehouse[MAXWH]; char isbn[MAXISBN]; char desc[MAXDESC]; char extra[MAXEXT];
} Record;
#pragma pack(pop)

typedef struct { char name[MAXN]; char pid[MAXPID]; long doff; long next; } ProdIdx;
typedef struct { char name[MAXC]; long fprod; long next; } CityIdx;
typedef struct { char name[MAXC]; long fcity; } CountryIdx;
typedef struct {
    CountryIdx co[MAXCOUNTRIES]; int nco;
    CityIdx ci[MAXCITIES]; int nci;
    ProdIdx pr[MAXPRODS]; int npr; int nrec;
} Idx;

static void scpy(char *d,const char *s,size_t n){strncpy(d,s,n-1);d[n-1]=0;}
static int icmp(const char *a,const char *b){
    while(*a&&*b){int ca=tolower((unsigned char)*a),cb=tolower((unsigned char)*b);
    if(ca!=cb)return ca-cb;a++;b++;}return(unsigned char)*a-(unsigned char)*b;}

static int load_json(Idx *ix){
    json_buf=jload_file(JSONFILE);
    if(!json_buf)return 0;
    jp=json_buf;
    FILE *df=fopen(DATFILE,"wb");
    if(!df){free(json_buf);return 0;}
    ix->nco=ix->nci=ix->npr=ix->nrec=0;
    jskip_ws();
    if(*jp!='['){fclose(df);free(json_buf);return 0;}
    jp++;
    while(*jp&&*jp!=']'){
        jskip_ws(); if(*jp==','){jp++;jskip_ws();}
        if(*jp!='{')break;
        jp++;
        char cname[MAXC]="",ccode[8]="";
        char *saved=jp;
        if(jfind_key("country"))jparse_str(cname,MAXC);
        jp=saved;
        if(jfind_key("country_code"))jparse_str(ccode,8);
        jp=saved;

        if(!jfind_key("cities")){jskip_val();jskip_ws();if(*jp=='}')jp++;continue;}
        jskip_ws();
        if(*jp!='['){jskip_val();jskip_ws();if(*jp=='}')jp++;continue;}
        jp++;
        int coi=-1;
        for(int i=0;i<ix->nco;i++)if(icmp(ix->co[i].name,cname)==0){coi=i;break;}
        if(coi<0){coi=ix->nco++;scpy(ix->co[coi].name,cname,MAXC);ix->co[coi].fcity=SENTINEL;}
        while(*jp&&*jp!=']'){
            jskip_ws();if(*jp==','){jp++;jskip_ws();}
            if(*jp!='{')break;
            jp++;
            char cityname[MAXC]="";
            char *csaved=jp;
            if(jfind_key("city_name"))jparse_str(cityname,MAXC);
            jp=csaved;
            int cii=-1;
            for(int i=0;i<ix->nci;i++){
                if(icmp(ix->ci[i].name,cityname)==0){
                    long c2=ix->co[coi].fcity;
                    while(c2!=SENTINEL){if(c2==i){cii=i;break;}c2=ix->ci[c2].next;}
                    if(cii>=0)break;
                }
            }
            if(cii<0){
                cii=ix->nci++;scpy(ix->ci[cii].name,cityname,MAXC);
                ix->ci[cii].fprod=SENTINEL;ix->ci[cii].next=ix->co[coi].fcity;ix->co[coi].fcity=cii;
            }

            if(!jfind_key("products")){

                int d=1;while(*jp&&d>0){if(*jp=='{')d++;else if(*jp=='}')d--;jp++;}
                continue;
            }
            jskip_ws();
            if(*jp!='['){jskip_val();int d=1;while(*jp&&d>0){if(*jp=='{')d++;else if(*jp=='}')d--;jp++;}continue;}
            jp++;
            while(*jp&&*jp!=']'){
                jskip_ws();if(*jp==','){jp++;jskip_ws();}
                if(*jp!='{')break;
                jp++;
                Record r;memset(&r,0,sizeof(r));
                scpy(r.country,cname,MAXC);scpy(r.ccode,ccode,8);scpy(r.city,cityname,MAXC);
                char *psaved=jp;
                if(jfind_key("product_id"))jparse_str(r.pid,MAXPID);
                jp=psaved;
                if(jfind_key("product_info")){
                    jskip_ws();if(*jp=='{'){jp++;
                    char *pi_s=jp;
                    if(jfind_key("name"))jparse_str(r.name,MAXN);
                    jp=pi_s;if(jfind_key("brand"))jparse_str(r.brand,MAXB);
                    jp=pi_s;if(jfind_key("category"))jparse_str(r.category,MAXCAT);
                    jskip_ws();while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
                    }
                }
                jp=psaved;
                if(jfind_key("pricing")){
                    jskip_ws();if(*jp=='{'){jp++;
                    char *pr_s=jp;
                    if(jfind_key("price")){jskip_ws();r.price=jparse_int();}
                    jp=pr_s;if(jfind_key("currency"))jparse_str(r.currency,MAXCUR);
                    jskip_ws();while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
                    }
                }
                jp=psaved;
                if(jfind_key("inventory")){
                    jskip_ws();if(*jp=='{'){jp++;
                    char *in_s=jp;
                    if(jfind_key("stock")){jskip_ws();r.stock=jparse_int();}
                    jp=in_s;if(jfind_key("warehouse")){jskip_ws();
                    if(strncmp(jp,"null",4)==0)jp+=4;else jparse_str(r.warehouse,MAXWH);}
                    jskip_ws();while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
                    }
                }
                jp=psaved;
                if(jfind_key("isbn")){jskip_ws();
                if(strncmp(jp,"null",4)==0)jp+=4;else jparse_str(r.isbn,MAXISBN);}
                jp=psaved;
                if(jfind_key("description"))jparse_str(r.desc,MAXDESC);
                jp=psaved;
                if(jfind_key("extra")){jskip_ws();
                if(strncmp(jp,"null",4)==0)jp+=4;else jparse_str(r.extra,MAXEXT);}

                while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
                long off=(long)ftell(df);
                fwrite(&r,sizeof(Record),1,df);
                int pi=ix->npr++;
                scpy(ix->pr[pi].name,r.name,MAXN);scpy(ix->pr[pi].pid,r.pid,MAXPID);
                ix->pr[pi].doff=off;ix->pr[pi].next=ix->ci[cii].fprod;ix->ci[cii].fprod=pi;
                ix->nrec++;
                jskip_ws();if(*jp==',')jp++;
            }
            if(*jp==']')jp++;
            jskip_ws();

            while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
            jskip_ws();if(*jp==',')jp++;
        }
        if(*jp==']')jp++;
        jskip_ws();
        while(*jp&&*jp!='}')jp++;if(*jp=='}')jp++;
        jskip_ws();if(*jp==',')jp++;
    }
    fclose(df);free(json_buf);json_buf=NULL;
    return 1;
}

static void sort_countries(Idx *ix){
    int n = ix->nco;
    if(n <= 1) return;

    CountryIdx inp[MAXCOUNTRIES];
    for(int i = 0; i < n; i++) inp[i] = ix->co[i];

    CountryIdx buf[MAXCOUNTRIES];
    int buf_pos = 0;
    int rstart[MAXCOUNTRIES+1];
    int nruns = 0;
    rstart[0] = 0; nruns = 1;

    int hs = HEAP_SZ;
    if(hs > n) hs = n;

    CountryIdx heap[HEAP_SZ];
    int dead[HEAP_SZ];
    int hsz = 0, ipos = 0;
    memset(dead, 0, sizeof(dead));

    for(int i = 0; i < hs && ipos < n; i++){
        heap[i] = inp[ipos++];
        hsz++;
    }

    #define CO_CMP(a,b) (dead[a]!=dead[b] ? dead[a]-dead[b] : icmp(heap[a].name, heap[b].name))
    #define CO_SIFT_DOWN(start, size) { \
        int _p = (start); \
        while(1){ \
            int _s=_p, _l=2*_p+1, _r=2*_p+2; \
            if(_l<(size) && CO_CMP(_l,_s)<0) _s=_l; \
            if(_r<(size) && CO_CMP(_r,_s)<0) _s=_r; \
            if(_s==_p) break; \
            CountryIdx _tmp=heap[_p]; heap[_p]=heap[_s]; heap[_s]=_tmp; \
            int _dd=dead[_p]; dead[_p]=dead[_s]; dead[_s]=_dd; \
            _p=_s; \
        } \
    }

    for(int i = hsz/2-1; i >= 0; i--){ CO_SIFT_DOWN(i, hsz); }

    char last_name[MAXC] = "";
    int alive = hsz;

    while(hsz > 0){
        if(alive == 0 || dead[0]){
            rstart[nruns++] = buf_pos;
            for(int i = 0; i < hsz; i++) dead[i] = 0;
            alive = hsz;
            last_name[0] = 0;
            for(int i = hsz/2-1; i >= 0; i--){ CO_SIFT_DOWN(i, hsz); }
        }

        buf[buf_pos++] = heap[0];
        scpy(last_name, heap[0].name, MAXC);

        if(ipos < n){
            heap[0] = inp[ipos++];
            dead[0] = 0;
            if(icmp(heap[0].name, last_name) < 0){
                dead[0] = 1;
                alive--;
            }
        } else {
            hsz--;
            if(hsz > 0){
                heap[0] = heap[hsz];
                dead[0] = dead[hsz];
            } else break;
        }
        CO_SIFT_DOWN(0, hsz);
    }
    rstart[nruns] = buf_pos;

    #undef CO_CMP
    #undef CO_SIFT_DOWN

    if(nruns == 1){
        for(int i = 0; i < n; i++) ix->co[i] = buf[i];
        return;
    }

    CountryIdx merged[MAXCOUNTRIES];
    int mpos = 0;
    int mheap_run[MAXCOUNTRIES];
    int cur_pos[MAXCOUNTRIES];
    int mhsz = 0;

    for(int r = 0; r < nruns; r++){
        cur_pos[r] = rstart[r];
        if(rstart[r] < rstart[r+1]) mheap_run[mhsz++] = r;
    }

    #define MG_CMP(a,b) icmp(buf[cur_pos[mheap_run[a]]].name, buf[cur_pos[mheap_run[b]]].name)
    #define MG_SIFT(p,sz) { \
        int _p=(p); \
        while(1){ \
            int _s=_p,_l=2*_p+1,_r=2*_p+2; \
            if(_l<(sz)&&MG_CMP(_l,_s)<0)_s=_l; \
            if(_r<(sz)&&MG_CMP(_r,_s)<0)_s=_r; \
            if(_s==_p)break; \
            int _t=mheap_run[_p];mheap_run[_p]=mheap_run[_s];mheap_run[_s]=_t; \
            _p=_s; \
        } \
    }

    for(int i = mhsz/2-1; i >= 0; i--){ MG_SIFT(i, mhsz); }

    while(mhsz > 0){
        int r = mheap_run[0];
        merged[mpos++] = buf[cur_pos[r]++];
        if(cur_pos[r] < rstart[r+1]){
            MG_SIFT(0, mhsz);
        } else {
            mhsz--;
            if(mhsz > 0){ mheap_run[0] = mheap_run[mhsz]; MG_SIFT(0, mhsz); }
        }
    }

    #undef MG_CMP
    #undef MG_SIFT

    for(int i = 0; i < n; i++) ix->co[i] = merged[i];
}

static void sort_city_list(Idx *ix, long *head){
    if(*head==SENTINEL) return;
    long inp[MAXCITIES]; int n=0;
    {long c=*head; while(c!=SENTINEL&&n<MAXCITIES){inp[n++]=c; c=ix->ci[c].next;}}
    if(n<=1) return;
    long buf[MAXCITIES]; int buf_pos=0;
    int rstart[MAXCITIES+1]; int nruns=0;
    rstart[0]=0; nruns=1;
    int hs=HEAP_SZ; if(hs>n)hs=n;
    long heap[HEAP_SZ]; int dead[HEAP_SZ]; int hsz=0,ipos=0;
    memset(dead,0,sizeof(dead));
    for(int i=0;i<hs&&ipos<n;i++){heap[i]=inp[ipos++];hsz++;}
    #define CI_CMP(a,b)(dead[a]!=dead[b]?dead[a]-dead[b]:icmp(ix->ci[heap[a]].name,ix->ci[heap[b]].name))
    #define CI_SIFT(st,sz){int _p=(st);while(1){int _s=_p,_l=2*_p+1,_r=2*_p+2;\
        if(_l<(sz)&&CI_CMP(_l,_s)<0)_s=_l; if(_r<(sz)&&CI_CMP(_r,_s)<0)_s=_r;\
        if(_s==_p)break; long _t=heap[_p];heap[_p]=heap[_s];heap[_s]=_t;\
        int _d=dead[_p];dead[_p]=dead[_s];dead[_s]=_d;_p=_s;}}
    for(int i=hsz/2-1;i>=0;i--){CI_SIFT(i,hsz);}
    char last[MAXC]=""; int alive=hsz;
    while(hsz>0){
        if(alive==0 || dead[0]){
            rstart[nruns++]=buf_pos;
            for(int i=0;i<hsz;i++)dead[i]=0; alive=hsz; last[0]=0;
            for(int i=hsz/2-1;i>=0;i--){CI_SIFT(i,hsz);}
        }
        buf[buf_pos++]=heap[0]; scpy(last,ix->ci[heap[0]].name,MAXC);
        if(ipos<n){
            heap[0]=inp[ipos++]; dead[0]=0;
            if(icmp(ix->ci[heap[0]].name,last)<0){dead[0]=1;alive--;}
        } else {
            hsz--; if(hsz>0){heap[0]=heap[hsz];dead[0]=dead[hsz];} else break;
        }
        CI_SIFT(0,hsz);
    }
    rstart[nruns]=buf_pos;
    #undef CI_CMP
    #undef CI_SIFT

    long out[MAXCITIES];
    if(nruns==1){
        for(int i=0;i<n;i++) out[i]=buf[i];
    } else {
        int mpos=0; int mheap_run[MAXCITIES]; int cur_pos[MAXCITIES]; int mhsz=0;
        for(int r=0;r<nruns;r++){cur_pos[r]=rstart[r]; if(rstart[r]<rstart[r+1])mheap_run[mhsz++]=r;}
        #define CMG_CMP(a,b) icmp(ix->ci[buf[cur_pos[mheap_run[a]]]].name, ix->ci[buf[cur_pos[mheap_run[b]]]].name)
        #define CMG_SIFT(p,sz) {int _p=(p);while(1){int _s=_p,_l=2*_p+1,_r=2*_p+2;\
            if(_l<(sz)&&CMG_CMP(_l,_s)<0)_s=_l; if(_r<(sz)&&CMG_CMP(_r,_s)<0)_s=_r;\
            if(_s==_p)break; int _t=mheap_run[_p];mheap_run[_p]=mheap_run[_s];mheap_run[_s]=_t;_p=_s;}}
        for(int i=mhsz/2-1;i>=0;i--){CMG_SIFT(i,mhsz);}
        while(mhsz>0){
            int r=mheap_run[0]; out[mpos++]=buf[cur_pos[r]++];
            if(cur_pos[r]<rstart[r+1]){CMG_SIFT(0,mhsz);}
            else{mhsz--;if(mhsz>0){mheap_run[0]=mheap_run[mhsz];CMG_SIFT(0,mhsz);}}
        }
        #undef CMG_CMP
        #undef CMG_SIFT
    }
    *head=out[0];
    for(int i=0;i<n-1;i++) ix->ci[out[i]].next=out[i+1];
    ix->ci[out[n-1]].next=SENTINEL;
}

static void sort_prod_list(Idx *ix, long *head){
    if(*head==SENTINEL) return;
    long inp[MAXPRODS]; int n=0;
    {long c=*head; while(c!=SENTINEL&&n<MAXPRODS){inp[n++]=c; c=ix->pr[c].next;}}
    if(n<=1) return;
    long buf[MAXPRODS]; int buf_pos=0;
    int rstart[MAXPRODS+1]; int nruns=0;
    rstart[0]=0; nruns=1;
    int hs=HEAP_SZ; if(hs>n)hs=n;
    long heap[HEAP_SZ]; int dead[HEAP_SZ]; int hsz=0,ipos=0;
    memset(dead,0,sizeof(dead));
    for(int i=0;i<hs&&ipos<n;i++){heap[i]=inp[ipos++];hsz++;}
    #define PR_CMP(a,b)(dead[a]!=dead[b]?dead[a]-dead[b]:icmp(ix->pr[heap[a]].name,ix->pr[heap[b]].name))
    #define PR_SIFT(st,sz){int _p=(st);while(1){int _s=_p,_l=2*_p+1,_r=2*_p+2;\
        if(_l<(sz)&&PR_CMP(_l,_s)<0)_s=_l; if(_r<(sz)&&PR_CMP(_r,_s)<0)_s=_r;\
        if(_s==_p)break; long _t=heap[_p];heap[_p]=heap[_s];heap[_s]=_t;\
        int _d=dead[_p];dead[_p]=dead[_s];dead[_s]=_d;_p=_s;}}
    for(int i=hsz/2-1;i>=0;i--){PR_SIFT(i,hsz);}
    char last[MAXC]=""; int alive=hsz;
    while(hsz>0){
        if(alive==0 || dead[0]){
            rstart[nruns++]=buf_pos;
            for(int i=0;i<hsz;i++)dead[i]=0; alive=hsz; last[0]=0;
            for(int i=hsz/2-1;i>=0;i--){PR_SIFT(i,hsz);}
        }
        buf[buf_pos++]=heap[0]; scpy(last,ix->pr[heap[0]].name,MAXC);
        if(ipos<n){
            heap[0]=inp[ipos++]; dead[0]=0;
            if(icmp(ix->pr[heap[0]].name,last)<0){dead[0]=1;alive--;}
        } else {
            hsz--; if(hsz>0){heap[0]=heap[hsz];dead[0]=dead[hsz];} else break;
        }
        PR_SIFT(0,hsz);
    }
    rstart[nruns]=buf_pos;
    #undef PR_CMP
    #undef PR_SIFT

    long out[MAXPRODS];
    if(nruns==1){
        for(int i=0;i<n;i++) out[i]=buf[i];
    } else {
        int mpos=0; int mheap_run[MAXPRODS]; int cur_pos[MAXPRODS]; int mhsz=0;
        for(int r=0;r<nruns;r++){cur_pos[r]=rstart[r]; if(rstart[r]<rstart[r+1])mheap_run[mhsz++]=r;}
        #define PMG_CMP(a,b) icmp(ix->pr[buf[cur_pos[mheap_run[a]]]].name, ix->pr[buf[cur_pos[mheap_run[b]]]].name)
        #define PMG_SIFT(p,sz) {int _p=(p);while(1){int _s=_p,_l=2*_p+1,_r=2*_p+2;\
            if(_l<(sz)&&PMG_CMP(_l,_s)<0)_s=_l; if(_r<(sz)&&PMG_CMP(_r,_s)<0)_s=_r;\
            if(_s==_p)break; int _t=mheap_run[_p];mheap_run[_p]=mheap_run[_s];mheap_run[_s]=_t;_p=_s;}}
        for(int i=mhsz/2-1;i>=0;i--){PMG_SIFT(i,mhsz);}
        while(mhsz>0){
            int r=mheap_run[0]; out[mpos++]=buf[cur_pos[r]++];
            if(cur_pos[r]<rstart[r+1]){PMG_SIFT(0,mhsz);}
            else{mhsz--;if(mhsz>0){mheap_run[0]=mheap_run[mhsz];PMG_SIFT(0,mhsz);}}
        }
        #undef PMG_CMP
        #undef PMG_SIFT
    }
    *head=out[0];
    for(int i=0;i<n-1;i++) ix->pr[out[i]].next=out[i+1];
    ix->pr[out[n-1]].next=SENTINEL;
}
static void build_sorted_index(Idx *ix){
    sort_countries(ix);
    for(int i=0;i<ix->nco;i++){sort_city_list(ix,&ix->co[i].fcity);
    long c=ix->co[i].fcity;while(c!=SENTINEL){sort_prod_list(ix,&ix->ci[c].fprod);c=ix->ci[c].next;}}
}

static int find_country(Idx *ix,const char *n){for(int i=0;i<ix->nco;i++)if(icmp(ix->co[i].name,n)==0)return i;return-1;}
static long find_city(Idx *ix,int ci,const char *n){long c=ix->co[ci].fcity;while(c!=SENTINEL){if(icmp(ix->ci[c].name,n)==0)return c;c=ix->ci[c].next;}return SENTINEL;}

static void print_record(Record *r){
    printf("  +----------------------------------------------------------+\n");
    printf("  | Product ID : %-41s |\n",r->pid);
    printf("  | Name       : %-41s |\n",r->name);
    printf("  | Brand      : %-41s |\n",r->brand);
    printf("  | Category   : %-41s |\n",r->category);
    printf("  | Price      : %-41d |\n",r->price);
    printf("  | Currency   : %-41s |\n",r->currency);
    printf("  | Stock      : %-41d |\n",r->stock);
    printf("  | Warehouse  : %-41s |\n",r->warehouse[0]?r->warehouse:"N/A");
    printf("  | ISBN       : %-41s |\n",r->isbn[0]?r->isbn:"N/A");
    printf("  | Country    : %-41s |\n",r->country);
    printf("  | City       : %-41s |\n",r->city);
    printf("  +----------------------------------------------------------+\n");
}

static void do_search(Idx *ix){
    char co[MAXC],ci[MAXC],pr[MAXN];
    printf("Enter country: ");fflush(stdout);fgets(co,MAXC,stdin);co[strcspn(co,"\n")]=0;


    printf("\n[TRACE] Scanning Country index (%d entries)...\n",ix->nco);
    int coi=-1;
    for(int i=0;i<ix->nco;i++){
        printf("[TRACE]   Country[%d] = \"%s\"",i,ix->co[i].name);
        if(icmp(ix->co[i].name,co)==0){
            coi=i;
            printf(" --> MATCH  (fcity=%ld)\n",ix->co[i].fcity);
            break;
        }
        printf("\n");
    }
    if(coi<0){printf("Country '%s' not found.\n",co);return;}

    printf("Enter city: ");fflush(stdout);fgets(ci,MAXC,stdin);ci[strcspn(ci,"\n")]=0;


    printf("\n[TRACE] Following City chain  head_idx=%ld\n",ix->co[coi].fcity);
    long c=ix->co[coi].fcity; long cii=SENTINEL;
    while(c!=SENTINEL){
        printf("[TRACE]   CityIdx[%ld] = \"%s\"  fprod=%ld  next=%ld",
               c,ix->ci[c].name,ix->ci[c].fprod,ix->ci[c].next);
        if(icmp(ix->ci[c].name,ci)==0){cii=c;printf(" --> MATCH\n");break;}
        printf("\n");
        c=ix->ci[c].next;
    }
    if(cii==SENTINEL){printf("City '%s' not found in %s.\n",ci,co);return;}

    printf("Enter product: ");fflush(stdout);fgets(pr,MAXN,stdin);pr[strcspn(pr,"\n")]=0;


    printf("\n[TRACE] Following Product chain  head_idx=%ld\n",ix->ci[cii].fprod);
    long p=ix->ci[cii].fprod; long pi=SENTINEL;
    while(p!=SENTINEL){
        printf("[TRACE]   ProdIdx[%ld] = \"%s\"  doff=%ld  next=%ld",
               p,ix->pr[p].name,ix->pr[p].doff,ix->pr[p].next);
        if(icmp(ix->pr[p].name,pr)==0){pi=p;printf(" --> MATCH\n");break;}
        printf("\n");
        p=ix->pr[p].next;
    }
    if(pi==SENTINEL){printf("Product '%s' not found in %s/%s.\n",pr,co,ci);return;}


    printf("\n[TRACE] Opening \"%s\", seeking to byte offset=%ld, reading %zu bytes...\n",
           DATFILE,ix->pr[pi].doff,sizeof(Record));
    FILE *f=fopen(DATFILE,"rb");if(!f){printf("Cannot open data file.\n");return;}
    fseek(f,ix->pr[pi].doff,SEEK_SET);Record r;fread(&r,sizeof(Record),1,f);fclose(f);
    printf("[TRACE] Record read successfully. No full-file scan performed.\n");
    printf("\n  === Found via: %s -> %s -> %s ===\n",co,ci,pr);
    print_record(&r);
}

static void do_display(Idx *ix){
    printf("\n  === MULTI-LEVEL INDEX ===\n\n");
    for(int i=0;i<ix->nco;i++){printf("  [Country] %s\n",ix->co[i].name);
    long c=ix->co[i].fcity;while(c!=SENTINEL){printf("    [City] %s\n",ix->ci[c].name);
    long p=ix->ci[c].fprod;while(p!=SENTINEL){printf("      [Product] %-20s (ID:%s off:%ld)\n",ix->pr[p].name,ix->pr[p].pid,ix->pr[p].doff);
    p=ix->pr[p].next;}c=ix->ci[c].next;}printf("\n");}
}
static void display_level(Idx *ix){
    int ch;printf("Select: 1)Countries 2)Cities 3)Products\nChoice: ");fflush(stdout);scanf("%d",&ch);getchar();
    if(ch==1){printf("\n=== COUNTRIES ===\n");for(int i=0;i<ix->nco;i++)printf("  %d. %s (first_city:%ld)\n",i+1,ix->co[i].name,ix->co[i].fcity);}
    else if(ch==2){printf("\n=== CITIES ===\n");for(int i=0;i<ix->nco;i++){printf("[%s]\n",ix->co[i].name);long c=ix->co[i].fcity;while(c!=SENTINEL){printf("  -> %s (idx:%ld fprod:%ld next:%ld)\n",ix->ci[c].name,c,ix->ci[c].fprod,ix->ci[c].next);c=ix->ci[c].next;}}}
    else if(ch==3){printf("\n=== PRODUCTS ===\n");for(int i=0;i<ix->nco;i++){long c=ix->co[i].fcity;while(c!=SENTINEL){printf("[%s/%s]\n",ix->co[i].name,ix->ci[c].name);long p=ix->ci[c].fprod;while(p!=SENTINEL){printf("  -> %-20s (idx:%ld doff:%ld next:%ld)\n",ix->pr[p].name,p,ix->pr[p].doff,ix->pr[p].next);p=ix->pr[p].next;}c=ix->ci[c].next;}}}
}

static void do_insert(Idx *ix){
    char co[MAXC],ci[MAXC],pid[MAXPID],nm[MAXN],br[MAXB],cat[MAXCAT],cur[MAXCUR],wh[MAXWH];int pr,st;
    printf("Country: ");fflush(stdout);fgets(co,MAXC,stdin);co[strcspn(co,"\n")]=0;
    int coi=find_country(ix,co);if(coi<0){printf("Country not found.\n");return;}
    printf("City: ");fflush(stdout);fgets(ci,MAXC,stdin);ci[strcspn(ci,"\n")]=0;
    long cii=find_city(ix,coi,ci);if(cii==SENTINEL){printf("City not found.\n");return;}
    printf("Product ID: ");fflush(stdout);fgets(pid,MAXPID,stdin);pid[strcspn(pid,"\n")]=0;
    printf("Name: ");fflush(stdout);fgets(nm,MAXN,stdin);nm[strcspn(nm,"\n")]=0;
    printf("Brand: ");fflush(stdout);fgets(br,MAXB,stdin);br[strcspn(br,"\n")]=0;
    printf("Category: ");fflush(stdout);fgets(cat,MAXCAT,stdin);cat[strcspn(cat,"\n")]=0;
    printf("Price: ");fflush(stdout);scanf("%d",&pr);getchar();
    printf("Currency: ");fflush(stdout);fgets(cur,MAXCUR,stdin);cur[strcspn(cur,"\n")]=0;
    printf("Stock: ");fflush(stdout);scanf("%d",&st);getchar();
    printf("Warehouse: ");fflush(stdout);fgets(wh,MAXWH,stdin);wh[strcspn(wh,"\n")]=0;
    Record r;memset(&r,0,sizeof(r));
    scpy(r.pid,pid,MAXPID);scpy(r.country,co,MAXC);scpy(r.city,ci,MAXC);
    scpy(r.name,nm,MAXN);scpy(r.brand,br,MAXB);scpy(r.category,cat,MAXCAT);
    r.price=pr;scpy(r.currency,cur,MAXCUR);r.stock=st;scpy(r.warehouse,wh,MAXWH);
    FILE *f=fopen(DATFILE,"ab");if(!f){printf("Cannot open data file.\n");return;}
    long off=(long)ftell(f);fwrite(&r,sizeof(Record),1,f);fclose(f);
    int pi=ix->npr++;scpy(ix->pr[pi].name,nm,MAXN);scpy(ix->pr[pi].pid,pid,MAXPID);
    ix->pr[pi].doff=off;ix->pr[pi].next=SENTINEL;
    long *head=&ix->ci[cii].fprod;
    if(*head==SENTINEL||icmp(nm,ix->pr[*head].name)<0){ix->pr[pi].next=*head;*head=pi;}
    else{long cx=*head;while(ix->pr[cx].next!=SENTINEL&&icmp(nm,ix->pr[ix->pr[cx].next].name)>0)cx=ix->pr[cx].next;
    ix->pr[pi].next=ix->pr[cx].next;ix->pr[cx].next=pi;}
    ix->nrec++;printf("Inserted '%s' into %s/%s.\n",nm,co,ci);
}

static void rss_demo(Idx *ix){
    printf("\n=== Replacement Selection Sort (Countries) ===\n\n");
    int n=ix->nco;if(n==0){printf("No data.\n");return;}
    char inp[MAXCOUNTRIES][MAXC];
    for(int i=0;i<n;i++)scpy(inp[i],ix->co[i].name,MAXC);
    for(int i=0;i<n/2;i++){char t[MAXC];strcpy(t,inp[i]);strcpy(inp[i],inp[n-1-i]);strcpy(inp[n-1-i],t);}
    printf("Input (reversed): ");for(int i=0;i<n;i++)printf("%s%s",inp[i],i<n-1?", ":"");printf("\n\n");
    int hs=HEAP_SZ;if(hs>n)hs=n;
    char hp[HEAP_SZ][MAXC];int hsz=0,dead[HEAP_SZ],ipos=0,rn=1;
    memset(dead,0,sizeof(dead));
    for(int i=0;i<hs&&ipos<n;i++){scpy(hp[i],inp[ipos++],MAXC);hsz++;}
    #define CMP_H(a,b)(dead[a]!=dead[b]?dead[a]-dead[b]:icmp(hp[a],hp[b]))
    #define SIFT(sz){for(int _i=(sz)/2-1;_i>=0;_i--){int p=_i;while(1){int s=p,l=2*p+1,r2=2*p+2;\
    if(l<(sz)&&CMP_H(l,s)<0)s=l;if(r2<(sz)&&CMP_H(r2,s)<0)s=r2;if(s==p)break;\
    char _t[MAXC];memcpy(_t,hp[p],MAXC);memcpy(hp[p],hp[s],MAXC);memcpy(hp[s],_t,MAXC);\
    int _d=dead[p];dead[p]=dead[s];dead[s]=_d;p=s;}}}
    SIFT(hsz);
    char last[MAXC]="";int alive=hsz;
    printf("--- Run %d ---\n",rn);
    while(hsz>0){
        if(alive==0){printf("\n--- Run %d ---\n",++rn);for(int i=0;i<hsz;i++)dead[i]=0;alive=hsz;last[0]=0;SIFT(hsz);}
        printf("%s",hp[0]);scpy(last,hp[0],MAXC);
        if(ipos<n){scpy(hp[0],inp[ipos++],MAXC);dead[0]=0;if(icmp(hp[0],last)<0){dead[0]=1;alive--;}}
        else{hsz--;if(hsz>0){memcpy(hp[0],hp[hsz],MAXC);dead[0]=dead[hsz];}else break;}
        {int p=0;while(1){int s=p,l=2*p+1,r2=2*p+2;
        if(l<hsz&&CMP_H(l,s)<0)s=l;if(r2<hsz&&CMP_H(r2,s)<0)s=r2;if(s==p)break;
        char _t[MAXC];memcpy(_t,hp[p],MAXC);memcpy(hp[p],hp[s],MAXC);memcpy(hp[s],_t,MAXC);
        int _d=dead[p];dead[p]=dead[s];dead[s]=_d;p=s;}}
        if(hsz>0&&!dead[0])printf(", ");
    }
    printf("\n\nDone. %d run(s) generated.\n",rn);
}

int main(void){
    static Idx ix; memset(&ix,0,sizeof(ix));
    printf("=============================================\n");
    printf("  Homework #2 - Multi-Level Indexing System\n");
    printf("  Student: 2023510135\n");
    printf("=============================================\n\n");
    printf("Loading %s ...\n",JSONFILE);
    if(!load_json(&ix)){fprintf(stderr,"Failed to load JSON.\n");return 1;}
    printf("Loaded %d records (%d countries, %d cities, %d products).\n",ix.nrec,ix.nco,ix.nci,ix.npr);
    printf("Building sorted index...\n");build_sorted_index(&ix);printf("Done.\n\n");
    int run=1;
    while(run){
        printf("=============================================\n");
        printf("  1. Search by Country/City/Product\n");
        printf("  2. Sort and Display Index Levels\n");
        printf("  3. Insert a New Product\n");
        printf("  4. Apply Replacement Selection Sort\n");
        printf("  5. Display Full Index Tree\n");
        printf("  6. Exit\n");
        printf("=============================================\n");
        printf("Choice: ");fflush(stdout);int ch;scanf("%d",&ch);getchar();printf("\n");
        switch(ch){
            case 1:do_search(&ix);break;case 2:display_level(&ix);break;
            case 3:do_insert(&ix);break;case 4:rss_demo(&ix);break;
            case 5:do_display(&ix);break;case 6:run=0;break;
            default:printf("Invalid option.\n");
        }
        if(run){
            printf("\nPress Enter to return to main menu...");
            fflush(stdout);
            getchar();
        }
        printf("\n");
    }
    printf("All resources cleaned up. Goodbye!\n");return 0;
}