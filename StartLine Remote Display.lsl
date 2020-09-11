//=====================================================================================
//Copyright Start Line 2005-Kanker Greenacre 2009-Cynthia Centaur 
//Copyright StartLine 2020-LaliaCasau
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
//About this script

//Parameters Config
integer pingChannel=       //***** Put channel, the channel to listen for chronometers and others objects 
//Texture Parameters sign and numbers
float charsize=0.25; //character width and height
vector charscale=<1.0,1.0,0.0>; //scale character
key tex;  //key texture
//faces number
integer TITLE=0;
integer SIGN=1;
integer HOUR=2;
integer COLON=3;
integer MIN10=4;
integer MIN1=5;
integer SEC10=6;
integer SEC1=7;
//others
string SIMBOLS="0123456789-+ '.:";
integer D=0;


integer n0; integer n1; integer n2; integer n3; integer n4; integer n5;
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
integer listenHandle;

//colors
integer colorchrono = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //light blue

integer currentLine;
string currentName;
key requestInFlight;

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

getLinkNums() 
{
    integer i;
    integer linkcount=llGetNumberOfPrims();  
    for (i=1;i<=linkcount;++i) {
        string str=llGetLinkName(i);
        if (str=="D") 
            D=i;
    }
} 

displayChrono(){
    integer now=llGetUnixTime();
    time=now-start_at;
    displayTime(1,time);
}

displayTime(integer pmode, integer ptime) {
    if (pmode==1 && colorchrono) {
        if (ptime<0) {
            if (ptime<-60) changeColor(color61);
            else if (ptime<-30) changeColor(color60);
            else if (ptime<-10) changeColor(color30);
            else changeColor(color10);
        } else {
            changeColor(colorstart);
        }
    } else {
        changeColor(colornormal);
    }
    if (ptime<0) n0=llSubStringIndex(SIMBOLS,"-");
    else n0=llSubStringIndex(SIMBOLS,"+");
    ptime=llAbs(ptime);
    hours=ptime/3600;
    ptime-=hours*3600;
    minutes=ptime/60;
    seconds=ptime%60;
    n1=llSubStringIndex(SIMBOLS,(string)(hours%10));
    n2=llSubStringIndex(SIMBOLS,(string)(minutes/10));
    n3=llSubStringIndex(SIMBOLS,(string)(minutes%10));
    n4=llSubStringIndex(SIMBOLS,(string)(seconds/10));
    n5=llSubStringIndex(SIMBOLS,(string)(seconds%10));
    llSetLinkPrimitiveParamsFast(LINK_THIS,[
        PRIM_TEXTURE,SIGN,tex,charscale,<(n0%4)*charsize, (3-n0/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,HOUR,tex,charscale,<(n1%4)*charsize, (3-n1/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,MIN10,tex,charscale,<(n2%4)*charsize, (3-n2/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,MIN1,tex,charscale,<(n3%4)*charsize, (3-n3/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SEC10,tex,charscale,<(n4%4)*charsize, (3-n4/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SEC1,tex,charscale,<(n5%4)*charsize, (3-n5/4)*charsize,0.0>,0.0
    ]);
}

changeColor(vector colorVec) {
    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colorVec,visible,PRIM_COLOR,HOUR,colorVec,visible,PRIM_COLOR,MIN1,colorVec,visible,PRIM_COLOR,MIN10,colorVec,visible,PRIM_COLOR,SEC1,colorVec,visible,PRIM_COLOR,SEC10,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
}

getTextures()
{
        list l=llGetLinkPrimitiveParams(D,[PRIM_DESC]);
        string desc=llList2String(l,0);
        key t1=(key)desc;
        l=llGetLinkPrimitiveParams(LINK_THIS,[PRIM_TEXTURE,3]);
        key k=llList2Key(l,0);
        if (k){ 
            if(t1!=k && D>0) llSetLinkPrimitiveParams(D,[PRIM_DESC,(string)k]);
            t1=k;
        }else if (t1) {       
        }else llSay(0,"Display numbers texture is not available, load default textures");

        if (t1) {
            llSetLinkPrimitiveParamsFast(1,[PRIM_TEXTURE,1,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,2,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,3,k,<1.0,1.0,0.0>,<0.74,0.0,0.0>,0.0,PRIM_TEXTURE,4,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,5,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,6,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,7,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
            tex=k; 
        }
}

updateTextures(string t1, string t2)  //Title, Clock numbers   textures
{
    t1=llStringTrim(t1, STRING_TRIM); 
    t2=llStringTrim(t2, STRING_TRIM); 
    key k;
    if (t1!="") {
        k=(key)t1;
        if (k) llSetLinkPrimitiveParamsFast(1,[PRIM_TEXTURE,0,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
    }
    if (t2!="") { 
        k=(key)t2;
        if (k) {
            llSetLinkPrimitiveParamsFast(1,[PRIM_TEXTURE,1,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,2,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,3,k,<1.0,1.0,0.0>,<0.74,0.0,0.0>,0.0,PRIM_TEXTURE,4,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,5,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,6,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,7,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
            tex=k; 
            if (D>0) llSetLinkPrimitiveParamsFast(D,[PRIM_DESC,(string)k]);
            displayTime(0,time);
        }
    }
}

inicio() {
    listenHandle = llListen(pingChannel, "", "", "");
    getTextures();
    displayTime(0,time);
}

default 
{
    
    state_entry() {
        startTime=0;
        time=startTime;
        displayTime(0,time);
        llSetStatus(STATUS_PHANTOM,TRUE);
        llSetTimerEvent(0);
        getLinkNums();
        readfirstNCLine("settings_rd");
        //llOwnerSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
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
    
    listen(integer pchannel, string name, key id, string str) { 
        if (pchannel!=pingChannel) return;   //only pingChannel is supported

        list li=llParseString2List(str,["|"],[]);
        string s=llList2String(li,0);
        if(s=="resetline"){
            llResetScript();
            return;
        }

        integer num=(integer)llList2String(li,1);
        if(s=="initline"){
            time=num;
            displayTime(0,time);
            return;
        }
        if(s=="startline"){
            start_at=num;
            displayChrono();
            llSetTimerEvent(1);
            return;
        }
        if(s=="restartline"){
            time=num;
            displayTime(0,time);
            changeColor(colornormal);
            llSetTimerEvent(0);
            return;
        }
        if(s=="visline"){
            visible=num;
            if (num==0) llSetAlpha(0.0,ALL_SIDES);
            else llSetAlpha(1.0,ALL_SIDES);
            return;
        }
        if(s=="colorsline"){
            colorchrono = (integer)llList2String(li,1);
            color61 = (vector)llList2String(li,2);
            color60 = (vector)llList2String(li,3);
            color30 = (vector)llList2String(li,4);
            color10 = (vector)llList2String(li,5);
            colorstart = (vector)llList2String(li,6);
            colornormal = (vector)llList2String(li,7);
            return;
        }
        if(llGetSubString(str,0,9)=="updtextext"){
            updateTextures(llList2String(li,1),llList2String(li,2));
        } 
    }
    
    timer() {
        displayChrono();
    }
}
