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

integer pingChannel=-2256845;
integer D;

//==============> START Chrono variables
float charsize=0.0820312; //character width and height
vector charinit=<0,0.917,0.097656>;
vector charscale=<1.0,1.0,0.0>; //scale character 
key texnumbers="87a7adc0-8656-f64a-1628-f13f99f2070e";  //numbers texture
key texmode="93067bbb-9922-32a5-742d-cc289fb6790a";  //mode texture
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
integer listenHandle;
integer bright=2;
integer initTimer=0;

//colors
integer colorchrono = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //light blue
vector color;

integer currentLine;
string currentName;
key requestInFlight;
//<============== END Chrono variables
getLinkNums() 
{
    integer i;
    integer linkcount=llGetNumberOfPrims();  
    for (i=1;i<=linkcount;++i) {
        string str=llGetLinkName(i);  
        if (str=="D") D=i;
    }
}

inicio() {
    listenHandle = llListen(pingChannel, "", "", "");
    getTextures();
    displayTime(0,time);
}

getTextures()
{
        string desc=llList2String(llGetLinkPrimitiveParams(D,[PRIM_DESC]),0);
        list l=llCSV2List(desc);
        string t1=llList2String(l,0);
        string t2=llList2String(l,1);
        if((key)t1) texnumbers=(key)t1;        
        if((key)t2) texmode=(key)t2;        
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SIGN,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,HOUR,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,SPACE,texnumbers,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,MIN,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,COLON,texnumbers,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,SEC,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,SUBTITLE,texmode,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
}

updateTextures(string p1, string p2, string p3, float hscale, float vscale)  //Title, numbers, mode   textures  
{
    if((key)p1) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,0,(key)p1,<hscale,vscale,0.0>,<0.0,0.0,0.0>,0.0]);  //title
    if((key)p2){  //numbers
        texnumbers=(key)p2;
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SIGN,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,HOUR,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,SPACE,texnumbers,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,MIN,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,COLON,texnumbers,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,SEC,texnumbers,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
    }
    if((key)p3){  //mode
        texmode=(key)p3;
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
    }
    if (D>0){ 
        string desc=llList2String(llGetLinkPrimitiveParams(D,[PRIM_DESC]),0);
        if(desc!=(string)texnumbers+","+(string)texmode){
            llSetLinkPrimitiveParamsFast(D,[PRIM_DESC,(string)texnumbers+","+(string)texmode]);
        }
    }
    displayTime(0,time);
}

//==============> START Read Notecard
readfirstNCLine(string name) 
// Start reading a notecard line. Only call this when this notecard exists
{
    currentLine=0; 
    currentName=name;
    if (llGetInventoryType(name)!=INVENTORY_NOTECARD){
        llSay(0, "Unable to find the '"+name+"' notecard. Default settings will be loaded.");
        inicio();
    }else{ 
        requestInFlight=llGetNotecardLine(currentName, currentLine); 
    }
}

readNextNCLine(string line) 
{
    string s;
    integer n=llSubStringIndex(line, "#");
    if (n!=0) { 
        if (n>0) s=llGetSubString(line,0,n-1);
        else s=line;
        s=llStringTrim(s, STRING_TRIM);
        if (s!="") {
            if (llSubStringIndex(s,"=")>0) parseKeyValue(s);
            else llSay(0, "malformed line: " + s);
        }
    }
    currentLine++;
    requestInFlight=llGetNotecardLine(currentName, currentLine);
}

parseKeyValue(string pText) 
{
    list parts=llParseString2List(pText, ["="], []);
    if (llGetListLength(parts)!=2) {
        return;
    }

    string keystring=llList2String(parts, 0);
    keystring=llToLower(llStringTrim(keystring, STRING_TRIM));
    string value=llList2String(parts, 1);
    value=llStringTrim(value, STRING_TRIM);

    if (keystring=="pingchannel") {
        if ((integer)value!=0) pingChannel=(integer)value;
        return;
    }
    llSay(0, "Settings: Unknown keyword: \""+keystring+"\"");    
}
//<============== END Read Notecard
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
                //change color a SIGN and HOUR
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

    if(pType>10) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,pType-10,colorVec,visible]);
    else if(pType==10) llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,colorVec,visible,PRIM_COLOR,2,colorVec,visible]);
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
        startTime=0;
        time=startTime;
        displayTime(0,time);
        llSetStatus(STATUS_PHANTOM,TRUE);
        llSetTimerEvent(0);
        getLinkNums();
        if(llGetInventoryType("StartLine settings_obj")!=INVENTORY_NONE) readfirstNCLine("StartLine settings_obj");
        else inicio();
    }
    
    dataserver( key queryid, string data ) {
        if (queryid == requestInFlight){
            if (data == EOF){
                requestInFlight = NULL_KEY;
                inicio();
            }else{
                readNextNCLine(data);
            }
        }        
    }    
    
    
    //listen(integer channel,string name,key id,string message ) {
    //    displayTime((integer)message);
    //}
 
    timer() {
        //llOwnerSay((string)llGetUnixTime());
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
    
    listen(integer pchannel, string name, key id, string str) 
    {
        //llOwnerSay("remote display: "+str);
        if(llGetOwnerKey(id)!=llGetOwner()) return;
        
        list l=llCSV2List(str);
        string cmd=llList2String(l,0);

        if(cmd=="startline"){
            start_at=(integer)llList2String(l,2);
            if(lineMode==3){
                startN=(integer)llList2String(l,1);
                if(startN==1) startat1=start_at;
                else if(startN==2) startat2=start_at;
                else if(startN==3) startat3=start_at;
                else if(startN==4) startat4=start_at;
            }
            stage=0;
            displayChrono();
            if(bright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]); 
            initTimer=1;
            llSetTimerEvent(0.1);
            return;
        }

        if(cmd=="restartline"){
            time=(integer)llList2String(l,1);
            if(lineMode==3) startN=1;
            stage=-1;
            displayTime(0,time);
            changeColor(swType,colornormal);
            llSetTimerEvent(0);
            if(bright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]); 
            return;
        }

        if (cmd=="mode") {
            lineMode=(integer)llList2String(l,1);
            if(lineMode==1) stage=-1;
            else if(lineMode==6) nettype=" ";
            llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_TEXTURE,SUBTITLE,texmode,charscale,<0,-0.125*lineMode,0>,0]);
            //llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SUBTITLE,colornormal,visible]);
            return;
        }

        if(cmd=="dispchron"){
            if(lineMode==3) startN=(integer)llList2String(l,2);
            else if(lineMode==6) nettype=llList2String(l,2);
            time=(integer)llList2String(l,3);
            displayTime(0,time);
            changeColor(swType,colornormal);
            if(llList2String(l,1)=="1") changeColor(10,<0,1,0>);   //paint sign green
            llSetTimerEvent(0);
            return;
        }

        if(cmd=="startchron"){  //restart display
            if(lineMode==3){ 
                startN=(integer)llList2String(l,1);
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
        
        if(cmd=="initline"){
            llSetLinkPrimitiveParamsFast(LINK_THIS,[
                PRIM_TEXTURE,SPACE,texnumbers,charscale,<0.17,0.09766,0.0>,0.0,
                PRIM_TEXTURE,COLON,texnumbers,charscale,<0.17,0.09766,0.0>,0.0]);
            time=(integer)llList2String(l,1);
            stage=-1;
            displayTime(0,time);
            return;
        }
        if(cmd=="visline"){  
            if(llList2String(l,1)=="0") visible=0;
            else visible=1;
            llSetLinkAlpha(LINK_THIS,visible,ALL_SIDES);
            return;
        }
        if(cmd=="colorsline"){
            colorchrono = (integer)llList2String(l,2);
            color61 = (vector)llList2String(l,3);
            color60 = (vector)llList2String(l,4);
            color30 = (vector)llList2String(l,5);
            color10 = (vector)llList2String(l,6);
            colorstart = (vector)llList2String(l,7);
            colornormal = (vector)llList2String(l,8);
            return;
        }
        if(lineMode==1){
            if(cmd=="stage"){  
                stage=(integer)llList2String(l,1);;
                return;
            }
            if (cmd=="modedata") {
                time1=(integer)llList2String(l,1);
                time2=(integer)llList2String(l,2);
                time3=(integer)llList2String(l,3);
                return;
            }        
        }
        if(cmd=="updtextchrono"){
            updateTextures(llList2String(l,1),llList2String(l,2),llList2String(l,3),(float)llList2String(l,4),(float)llList2String(l,5));
            return;
        } 
        if(str=="resetline"){
            llResetScript();
            return;
        }
        if(cmd=="bright"){
            bright=(integer)llList2String(l,1);
            if(bright==2 || bright==0) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
            else llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
        }

    }    
    
    link_message(integer sender_num,integer num,string str,key data) {
        if(num>=0 && num<1000){   //link messages for all scripts
        }
        if(num<5000 || num>5999) return;   //chrono messages num between 5000 and 5999
        if(str=="changecol"){
            changeColor(num-5000,(vector)((string)data));
            return;
        }
    }
}
