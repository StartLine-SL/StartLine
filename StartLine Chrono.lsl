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

//colors
integer colorchrono = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //light blue
vector color;

//<============== END Chrono variables
//==============> START Auxiliar functions variables
integer LEFTLINE;
integer RIGHTLINE;

integer swWaiting;

//<============== END Auxiliar functions variables

dbgSay(string text)
{
    if(dbg!=0){ 
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
}

//==============> START Chrono functions
displayChrono(){
    integer now=llGetUnixTime();
    if(lineMode==2 || lineMode==7) time=start_at-now;
    else time=now-start_at;
    displayTime(1,time);
}

displayTime(integer pmode, integer ptime) {     //0 normal   1 prestart
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
            if(stage<0){ 
                n1=13;   //" "
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,1.0,PRIM_COLOR,HOUR,colornormal,1.0]);
            }else{ 
                n1=llSubStringIndex(SIMBOLS,(string)stage); //hours
                if(stage==0) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,0,0>,1.0,PRIM_COLOR,HOUR,<1,0,0>,1.0]);
                else if(stage==1) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<0,1,0>,1.0,PRIM_COLOR,HOUR,<0,1,0>,1.0]);
                else if(stage==2) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,1,0>,1.0,PRIM_COLOR,HOUR,<1,1,0>,1.0]);
                else if(stage==3) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,<1,0,0>,1.0,PRIM_COLOR,HOUR,<1,0,0>,1.0]);
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
            n0=llSubStringIndex(SIMBOLS,nettype); //space,S,F hour
            n1=13; //space hours
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
    changeColor(swType,color);
}

changeColor(integer pType, vector colorVec) {
    //paint all
    if(pType==0) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colorVec,visible,PRIM_COLOR,HOUR,colorVec,visible,PRIM_COLOR,MIN,colorVec,visible,PRIM_COLOR,SEC,colorVec,visible,PRIM_COLOR,SPACE,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
    //sign and hours default color
    else if(pType==1)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,visible,PRIM_COLOR,HOUR,colornormal,visible,PRIM_COLOR,MIN,colorVec,visible,PRIM_COLOR,SEC,colorVec,visible,PRIM_COLOR,SPACE,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
    //sign default color
    else if(pType==2)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colornormal,visible,PRIM_COLOR,HOUR,colorVec,visible,PRIM_COLOR,MIN,colorVec,visible,PRIM_COLOR,SEC,colorVec,visible,PRIM_COLOR,SPACE,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
    //sign and hours no change color
    else if(pType==3)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,MIN,colorVec,visible,PRIM_COLOR,SEC,colorVec,visible,PRIM_COLOR,SPACE,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
    //sign no paint
    else if(pType==4)  llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,HOUR,colorVec,visible,PRIM_COLOR,MIN,colorVec,visible,PRIM_COLOR,SEC,colorVec,visible,PRIM_COLOR,SPACE,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
    else if(pType>10) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,pType-10,colorVec,visible]);
}

//<============== END Chrono functions

//==============> START Auxiliar functions
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

//id,lockType,locktime,lockTimeDef,autolockType;
fautolock(string params)
{
    list l=llCSV2List(params);
    key id=(key)llList2String(l,0);
    integer lockType=(integer)llList2String(l,1);
    integer locktime=(integer)llList2String(l,2);
    integer lockTimeDef=(integer)llList2String(l,3);
    integer autolockType=(integer)llList2String(l,4);
    string s="";
    integer index=llListFindList(owners,[id]);
    if ((lockType==0) || (lockType==1 && locktime-llGetUnixTime()<lockTimeDef)) {
        if (autolockType==1 && index>=0){ 
            lockType=2;
            s="hard locked";
        }else{
            lockType=1;
            s="locked";
        }
    }else if (lockType==2 && locktime-llGetUnixTime()<lockTimeDef && autolockType==1 && index>=0) {
        lockType=2;
        s="hard locked";
    }
    if (s!=""){ 
        string lockName=llKey2Name(id);
        llMessageLinked(LINK_ALL_OTHERS, 7100, "locked", (string)id+","+s+","+lockName+","+(string)lockTimeDef);
        llMessageLinked(LINK_ALL_OTHERS, 1000+lockType, "autolock", (string)id);
    } else {
        llMessageLinked(LINK_ALL_OTHERS, 1000+lockType, "autolock", "");
    }
}

//<============== END Auxiliar functions

default {
    
    state_entry() {
        //llListen(5910798,"",NULL_KEY,"");
        startTime=0;
        time=startTime;
        owners=[llGetOwner()];

        //llSetStatus(STATUS_PHANTOM,TRUE);
        llSetTimerEvent(0);
    }
    
    //listen(integer channel,string name,key id,string message ) {
    //    displayTime((integer)message);
    //}
 
    timer() {
        displayChrono();
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
                llSetTimerEvent(1);
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
            if(str=="visible"){  
                if(num==0) visible=0;
                else visible=1;
                return;
            }
            if (str=="modedata") {
                if(lineMode==1){
                    list li=llCSV2List(data);
                    time1=(integer)llList2String(li,0);
                    time2=(integer)llList2String(li,1);
                    time3=(integer)llList2String(li,2);
                }
                return;
            }
            if (str=="mode") {
                lineMode=num;
                if(lineMode==1) stage=-1;
                else if(lineMode==6) nettype=" ";
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,charscale,<0,-0.125*lineMode,0>,0]);
                //llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SUBTITLE,colornormal,visible]);
                return;
            }
            if(str=="reset"){
                llResetScript();
                return;
            }
            if (str=="config") {
                list l=llCSV2List(data); 
                dbg=(integer)llList2String(l,8);
                dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                return;
            }
            return;
        }
        if(num<5000 || num>5999) return;   //chrono messages num between 5000 and 5999

        if(str=="init"){
            llSetLinkPrimitiveParamsFast(LINK_THIS,[
                PRIM_TEXTURE,SPACE,texnumbers,charscale,<0.17,0.09766,0.0>,0.0,
                PRIM_TEXTURE,COLON,texnumbers,charscale,<0.17,0.09766,0.0>,0.0]);
            time=(integer)((string)data);
            stage=-1;
            displayTime(0,time);
            return;
        }
        if(str=="dispchron"){  //reset display
            if(lineMode==3) startN=(integer)llGetSubString((string)data,0,0);
            else if(lineMode==6) nettype=llGetSubString((string)data,0,0);
            time=(integer)llGetSubString((string)data,2,-1);
            displayTime(0,time);
            changeColor(swType,colornormal);
            if(num==5001) changeColor(11,<0,1,0>);   //paint sign green
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
                llSetTimerEvent(1);
            }
            return;
        }
        if(str=="changecol"){
            changeColor(num-5000,(vector)((string)data));
            return;
        }
        if(lineMode==1){
            if(str=="stage"){  
                stage=num-5000;
                return;
            }
        }
        if(llGetSubString(str,0,5)=="colors"){
            list l=llParseString2List(str,["|"],[]);
            colorchrono = (integer)llList2String(l,1);
            color61 = (vector)llList2String(l,2);
            color60 = (vector)llList2String(l,3);
            color30 = (vector)llList2String(l,4);
            color10 = (vector)llList2String(l,5);
            colorstart = (vector)llList2String(l,6);
            colornormal = (vector)llList2String(l,7);
            return;
        }
        if(llGetSubString(str,0,12)=="updtextchrono"){
            integer n=llSubStringIndex(data,",");
            if(n>0){
                texnumbers=(key)llGetSubString(data,0,n-1);
                texmode=(key)llGetSubString(data,n+1,-1);
                //llOwnerSay((string)texnumbers+"      "+(string)texmode);
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,charscale,<0,-0.125*lineMode,0>,0]);
            }else texnumbers=data;
            displayTime(0,time);
            return;
        } 
        if(str=="autolock"){
            fautolock((string)data);
            return;
        }
    }
}
