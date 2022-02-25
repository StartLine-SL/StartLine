//=====================================================================================
//Copyright Start Line 2005-Kanker Greenacre 2009-Cynthia Centaur 
//Copyright StartLine  2020-LaliaCasau
//GPL-3.0-or-later
//This file is part of 'StartLine'.
//
//'StartLine' is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//'StartLine' is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with 'StartLine'.  If not, see <https://www.gnu.org/licenses/>.
//=====================================================================================
//About this script - StartLine Chrono
//5000-5999 Chrono

integer dbg;
integer model; 

//==============> START Chrono variables
float charsize=0.0820312; //character width and height
vector charinit=<0,0.917,0.097656>;
vector charscale=<1.0,1.0,0.0>; //scale character 
key texnumbers;  //numbers texture
key texmode;  //mode texture
//faces number
integer TITLE=0;
integer SIGN=1;
integer HOUR=2;
integer SPACE=3;
integer MIN=4;
integer COLON=5;
integer SEC=6;
integer SUBTITLE=7;
//others
string SIMBOLS="+-:._?RSFWXXX 0123456789";

integer LEFTLINE=0;
integer RIGHTLINE=0;

integer n0; integer n1; integer n2; integer n3; integer n4; float nf1; float nf2; float nf3;
integer i;
integer j;
integer length;
string letter;
key self;
string timeStr;
integer time;
integer startTime;
integer hours;
integer minutes;
integer seconds;
integer start_at;
integer visible=1;
integer lineMode=0;
integer time1;
integer time2;
integer time3;
integer startN=1;
list ltemp;
integer swType;
integer startat1;
integer startat2;
integer startat3;
integer startat4;
string nettype;
integer stage;
list owners;

string flyNum="9000";

//colors
integer colorline=1;
integer colorchrono = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //light blue
vector color;
vector colorv;

integer swReady;
integer vvisible;
float alphaline;
float valpha;
integer initTimer=0;
//<============== END Chrono variables
//==============> START Auxiliar functions variables

//<============== END Auxiliar functions variables

dbgSay(string text)
{
    if(dbg!=0){ 
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
}

getLinkNums() 
{
    integer i;
    integer linkcount=llGetNumberOfPrims();  
    for (i=1;i<=linkcount;++i) {
        string str=llGetLinkName(i);
        if (str=="leftline") LEFTLINE=i;
        else if (str=="rightline") RIGHTLINE=i;
    }
} 

//==============> START Chrono functions
displayChrono(){
    integer now=llGetUnixTime();
    if(lineMode==2 || lineMode==7) time=start_at-now;
    else time=now-start_at;
    //llOwnerSay("chrono display crhono "+(string)time+"   "+(string)now+"     "+(string)start_at);
    displayTime(1,time);
}

displayTime(integer pmode, integer ptime) {     //0 normal   1 prestart
//llOwnerSay("chrono displaytime "+(string)pmode+"   "+(string)ptime+"   "+(string)colorchrono);
    colorv=color;
    if (pmode==1 && colorchrono) {
        if (ptime<0) {
            if(lineMode==1){
                if (ptime<-time3) color=color61;   //init CD   >240
                else if (ptime<-time2) color=color60; //open Ocs  60-240
                else if (ptime<-10) color=color30; //close Ocs  10-60
                else color=color10;  //lesser 10
            }else if(lineMode==2){
                color=color61;
            }else if(lineMode==7){
                color=color61;
            }else{
                if (ptime<-60) color=color61;     //greater 60
                else if (ptime<-30) color=color60; //greater 30
                else if (ptime<-10) color=color30; //greater 10
                else color=color10; //lesser 10
            }
        } else {
            if(lineMode==2){
                if (ptime>120) color=colorstart;   //init CD   >240
                else if (ptime>60) color=color60; //open Ocs  60-240
                else if (ptime>30) color=color30; //close Ocs  10-60
                else color=color10;  //lesser 10
            }else if(lineMode==7){
                if (ptime>120) color=colorstart;   //init CD   >240
                else if (ptime>60) color=color60; //open Ocs  60-240
                else if (ptime>30) color=color30; //close Ocs  10-60
                else color=color10;  //lesser 10
            }else{
                color=colorstart;
            }
        }
    } else {
        color=colornormal;
    }
    n4=llAbs(ptime);
    hours=n4/3600;
    n4-=hours*3600;
    minutes=n4/60;
    seconds=n4%60;
    if(ptime<0){
        if(lineMode==1){  //match mode
            swType=3;
            n0=7;  //S sign
            if(swReady) vvisible=visible;
            else vvisible=0;
            if(stage<0){ 
                n1=13;   //" "
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,vvisible,PRIM_COLOR,HOUR,colornormal,vvisible]);
            }else{ 
                n1=llSubStringIndex(SIMBOLS,(string)stage); //hours
                if(stage==0) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,0,0>,vvisible,PRIM_COLOR,HOUR,<1,0,0>,vvisible]);
                else if(stage==1) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<0,1,0>,vvisible,PRIM_COLOR,HOUR,<0,1,0>,vvisible]);
                else if(stage==2) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,1,0>,vvisible,PRIM_COLOR,HOUR,<1,1,0>,vvisible]);
                else if(stage==3) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,0,0>,vvisible,PRIM_COLOR,HOUR,<1,0,0>,vvisible]);
            }
            n2=1;  //-   space
            nf3=0.0;
            nf1=1.00421;
            nf2=0.989; //0.98707;
        }else if(lineMode==3){   //multistart
            swType=1;
            n0=7; //S sign
            n1=llSubStringIndex(SIMBOLS,(string)startN); //hour
            n2=1;  //-   space
            nf3=0.0;
            nf1=1.00421;
            nf2=0.989;
        }else if(lineMode==6){   //network mode
            swType=4;
            if(nettype!="T" && nettype!="G"){ 
                n0=llSubStringIndex(SIMBOLS,nettype); //space,S,F hour
                n1=13; //space hours
            }else{ //WS   WF
                n0=9;  //W
                if(nettype=="T") n1=7;     //S
                else if(nettype=="G") n1=8;   //F
            }
            n2=1;  //-   space
            nf3=0.0;
            nf1=0.0;
            nf2=0.0;
        }else{
            swType=0;
            n0=1; //"-" sign
            n1=llSubStringIndex(SIMBOLS,"0"); //" " hour
            n2=2;  //:   space
            nf3=0.00593;
            nf1=0.0;
            nf2=0.0;
        }
    }else{ 
        if(lineMode==3){
            swType=1;
            n0=7; //S sign
            n1=llSubStringIndex(SIMBOLS,(string)(startN)); //hour
            n2=13;  //espacio   space
            nf3=0.0;
            nf1=1.00421;
            nf2=0.989;
        }else if(lineMode==6){
            swType=4;
            n0=llSubStringIndex(SIMBOLS,nettype); //space,S,F hour
            n1=llSubStringIndex(SIMBOLS,(string)(hours%10)); //hour
            n2=2;  //:   space
            nf3=0.00593;
            nf1=0.0;
            nf2=0.0;
        }else{
            swType=0;
            if(hours>9) n0=llSubStringIndex(SIMBOLS,(string)(hours/10));
            else n0=0;  //+ sign
            n1=llSubStringIndex(SIMBOLS,(string)(hours%10));
            n2=2; //:  space
            nf3=0.00593;
            nf1=0.0;
            nf2=0.0;
        }
    }
    llSetLinkPrimitiveParamsFast(LINK_THIS,[
        PRIM_TEXTURE,SIGN,texnumbers,charscale,<charinit.x+(n0%12)*charsize-nf1,charinit.z-(n0/12)*charsize,0.0>,0.0,
        PRIM_TEXTURE,HOUR,texnumbers,charscale,<charinit.x+(n1%12)*charsize-nf2,charinit.z-(n1/12)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SPACE,texnumbers,charscale,<charinit.x+(n2%12)*charsize+nf3,charinit.z-(n2/12)*charsize,0.0>,0.0,
        PRIM_TEXTURE,MIN,texnumbers,charscale,<charinit.x+(minutes%6)*charsize*2,charinit.y-(minutes/6)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SEC,texnumbers,charscale,<charinit.x+(seconds%6)*charsize*2,charinit.y-(seconds/6)*charsize,0.0>,0.0
    ]);
    if(color!=colorv) changeColor(swType,color);
}

changeColor(integer pType, vector colorVec) {
    if(swReady){ 
        vvisible=visible;
        valpha=alphaline;
    }else{ 
        vvisible=0;
        valpha=0;
    } 
    
    if(pType==0) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colorVec,vvisible,PRIM_COLOR,HOUR,colorVec,vvisible,PRIM_COLOR,MIN,colorVec,vvisible,PRIM_COLOR,SEC,colorVec,vvisible,PRIM_COLOR,SPACE,colorVec,vvisible,PRIM_COLOR,COLON,colorVec,vvisible]);
    //sign and hours default color
    else if(pType==1)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,vvisible,PRIM_COLOR,HOUR,colornormal,vvisible,PRIM_COLOR,MIN,colorVec,vvisible,PRIM_COLOR,SEC,colorVec,vvisible,PRIM_COLOR,SPACE,colorVec,vvisible,PRIM_COLOR,COLON,colorVec,vvisible]);
    //sign default color
    else if(pType==2)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,vvisible,PRIM_COLOR,HOUR,colorVec,vvisible,PRIM_COLOR,MIN,colorVec,vvisible,PRIM_COLOR,SEC,colorVec,vvisible,PRIM_COLOR,SPACE,colorVec,vvisible,PRIM_COLOR,COLON,colorVec,vvisible]);
    //sign and hours no change color
    else if(pType==3)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,MIN,colorVec,vvisible,PRIM_COLOR,SEC,colorVec,vvisible,PRIM_COLOR,SPACE,colorVec,vvisible,PRIM_COLOR,COLON,colorVec,vvisible]);
    //sign no paint
    else if(pType==4)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,HOUR,colorVec,vvisible,PRIM_COLOR,MIN,colorVec,vvisible,PRIM_COLOR,SEC,colorVec,vvisible,PRIM_COLOR,SPACE,colorVec,vvisible,PRIM_COLOR,COLON,colorVec,vvisible]);

    if(pType<10){
        if(RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,colorVec,valpha,PRIM_COLOR,4,colorVec,0.0]);
        if(LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,colorVec,valpha,PRIM_COLOR,2,colorVec,0.0]);
    }else if(pType>10){  
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,pType-10,colorVec,vvisible]);
    }else{
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,colorVec,vvisible,PRIM_COLOR,2,colorVec,vvisible]);
    }
}

finit(integer ptime){
    llSetLinkPrimitiveParamsFast(LINK_THIS,[
        PRIM_TEXTURE,SPACE,texnumbers,charscale,<0.17,0.09766,0.0>,0.0,
        PRIM_TEXTURE,COLON,texnumbers,charscale,<0.17,0.09766,0.0>,0.0]);
    time=ptime;
    //stage=-1;
    displayTime(0,time);
}
fmode(integer pmode){
    lineMode=pmode;
    if(lineMode==1) stage=-1;
    else if(lineMode==6) nettype=" ";
    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,charscale,<0,-0.125*lineMode,0>,0]);
    //llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SUBTITLE,colornormal,visible]);
}

//<============== END Chrono functions
default {
    
    state_entry() {
        //llListen(5910798,"",NULL_KEY,"");
        startTime=0;
        time=startTime;
        owners=[llGetOwner()];
        getLinkNums();
        //llSetStatus(STATUS_PHANTOM,TRUE);
        llSetTimerEvent(0);
    }
    
    //listen(integer channel,string name,key id,string message ) {
    //    displayTime((integer)message);
    //}
 
    timer() {
        if(initTimer==0){ 
            displayChrono();
            return;
        }else if(initTimer==1){ 
            initTimer=llGetUnixTime();
            displayChrono();
        }else if(llGetUnixTime()>=initTimer+1){
            llSetTimerEvent(1.0);
            initTimer=0;
        }
    }
    
    link_message(integer sender_num,integer num,string str,key data) {
        if(num>=0 && num<1000){   //link messages for all scripts
            if(str=="start"){
                start_at=(integer)((string)data);
                if(lineMode==3){
                    startN=num;
                    if(startN==1) startat1=start_at;
                    else if(startN==2) startat2=start_at;
                    else if(startN==3) startat3=start_at;
                    else if(startN==4) startat4=start_at;
                }
                stage=0;
                displayChrono();
                initTimer=1;
                llSetTimerEvent(0.1);
                return;
            }
            
            if(str=="restart"){
                time=(integer)llGetSubString(data,37,-1);    //data= id,time
                if(lineMode==3) startN=1;
                stage=-1;
                displayTime(0,time);
                changeColor(swType,colornormal);
                llSetTimerEvent(0);
                return;
            }
            if(str=="racecount"){   //data=startN/stage/nettype,flyNum1,time   for fly model
                lineMode=num;
                start_at=(integer)llGetSubString(data,7,-1);
                if(lineMode==3){
                    startN=(integer)llGetSubString(data,0,0);
                    /*if(startN==1) startat1=start_at;
                    else if(startN==2) startat2=start_at;
                    else if(startN==3) startat3=start_at;
                    else if(startN==4) startat4=start_at;  */
                }else if(lineMode==1){
                    stage=(integer)llGetSubString(data,0,0);
                }else if(lineMode==6){
                    nettype=llGetSubString(data,0,0);
                }
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,COLON,texnumbers,charscale,<0.17,0.09766,0.0>,0.0]);
                displayChrono();
                if(lineMode==6 && nettype!=" ") changeColor(11,<0,1,0>);   //paint sign green
                initTimer=1;
                llSetTimerEvent(0.1);
                return;
            }
            
            if(str=="racerest"){     //data=startN/stage/nettype,flyNum1,time   for fly model
                llSetTimerEvent(0);
                lineMode=num;
                if(lineMode==3) startN=1;
                else if(lineMode==1) stage=0;
                else if(lineMode==6) nettype=llGetSubString(data,0,0);
                changeColor(swType,colornormal);
                finit((integer)llGetSubString(data,7,-1));
                if(lineMode==6){
                    if(nettype!=" ") changeColor(11,<0,1,0>);   //paint sign green
                    if(nettype=="G" || nettype=="T") changeColor(12,<0,1,0>);   //paint hour green
                }
                return;
            }
            if(str=="visible"){  
                if(num==0) visible=0;
                else visible=1;
                return;
            }
            if (str=="modedata") {
                list li=llCSV2List(data);
                time1=(integer)llList2String(li,0);
                time2=(integer)llList2String(li,1);
                time3=(integer)llList2String(li,2);
                return;
            }
            if (str=="mode") {
                fmode(num);
                return;
            }
            if(str=="reset"){
                llResetScript();
                return;
            }
            if (str=="config") {
                list l=llCSV2List(data); 
                alphaline=(float)llList2String(l,6);
                dbg=(integer)llList2String(l,8);
                model=(integer)llList2String(l,14);
                if(model==0) swReady=1;
                if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                return;
            }
            if (str=="confly") {  //config from startlinefly for model=1
                list l=llCSV2List(data); 
                dbg=(integer)llList2String(l,2);
                alphaline=(float)llList2String(l,5);
                model=(integer)llList2String(l,6);
                if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                return;
            }
            return;
        }
        if(num<5000 || num>5999) return;   //chrono messages num between 5000 and 5999

        if(str=="init"){
            finit((integer)((string)data));
            return;
        }
        if(str=="dispchron"){  //reset display
            if(lineMode==3) startN=(integer)llGetSubString((string)data,0,0);
            else if(lineMode==6) nettype=llGetSubString((string)data,0,0);
            time=(integer)llGetSubString((string)data,2,-1);
            displayTime(0,time);
            changeColor(swType,colornormal);
            if(num==5001) changeColor(10,<0,1,0>);   //paint sign green
            llSetTimerEvent(0);
            return;
        }
        if(str=="startchron"){  //restart display
            if(lineMode==3){ 
                startN=(integer)((string)data);
                if(startN==1) start_at=startat1;
                else if(startN==2) start_at=startat2;
                else if(startN==3) start_at=startat3;
                else if(startN==4) start_at=startat4;
                displayChrono();
                initTimer=1;
                llSetTimerEvent(0.1);
            }
            return;
        }
        if(str=="changecol"){
            changeColor(num-5000,(vector)((string)data));
            return;
        }
        if(str=="startats"){  //load startats for multistart
            list l=llCSV2List((string)data);
            startat1=(integer)llList2String(l,0);
            startat2=(integer)llList2String(l,1);
            startat3=(integer)llList2String(l,2);
            startat4=(integer)llList2String(l,3);
            return;
        }
        if(lineMode==1){
            if(str=="stage"){  
                stage=num-5000;
                return;
            }
        }
        if(str=="colors"){
            list l=llCSV2List((string)data);
            colorline=(integer)llList2String(l,0);
            colorchrono=(integer)llList2String(l,1);
            color61=(vector)llList2String(l,2);
            color60=(vector)llList2String(l,3);
            color30=(vector)llList2String(l,4);
            color10=(vector)llList2String(l,5);
            colorstart=(vector)llList2String(l,6);
            colornormal=(vector)llList2String(l,7);
            return;
        }
        if(str=="run"){
            swReady=(integer)((string)data);
            return;
        }
        if(llGetSubString(str,0,12)=="updtextchrono"){
            integer n=llSubStringIndex(data,",");
            if(n>0){
                texnumbers=(key)llGetSubString(data,0,n-1);
                texmode=(key)llGetSubString(data,n+1,-1);
                integer vlm=lineMode;
                if(num>5000) vlm=num-5001;  //for startline fly
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,charscale,<0,-0.125*vlm,0>,0]);
            }else texnumbers=data;
            displayTime(0,time);
            return;
        } 
    }
}
