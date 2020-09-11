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
//About this script - StartLine Aux

integer dbg=0;
integer model=0;    //0 es la normal   162 es la linea de 162 metros
integer updtextChannel=       //***** Put channel, Communication channel with the design kit 
integer channel=              //***** Put channel, The channel to listen for start, reset and miscellanous other commands for nav huds
integer pingChannel=          //***** Put channel, the channel to listen for chronometers and others objects 
integer region_relay_channel=0;     //Load from setting notecard this is where the line will relay information normally said in chat Default is 0 No relay information
//Global Variables/////////////////////////////////////////////
//read Notecard
integer currentLine;
key requestInFlight;
string currentName;
//settings notecard
float alphaline=0.5;
integer setDName=1;   //In the messages it says the legacy name (0) or the display name (1)
integer autolock=1;   //automatically locks when starting the race
integer lockTimeDef=1800;  //default autolock time if auto lock is activated sec
integer autolockType=1;  //default autolock type   0 softlock   1 hardlock
integer colorchrono = 1;
integer colorline = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //white
list owners;
integer visibleleft=1;
integer visibleright=1;
float alphaleft;
float alpharight;
integer startsay;
integer menuoutregion=0;
integer autounlock=1;
integer laptimessay=0;

integer visible=1;
integer nMenuTime;
string  lockName="";
key  lockID=NULL_KEY;
integer locktime=-1; //time (seconds) the lockout ends  -1 no block
integer lockType=0;  //Block type  0 no block   1 soft block   2 hard block
key unlockID=NULL_KEY;

integer hr;
integer mn;
integer sc;
string hms;

//constants
vector COLOR_WHITE=<1.0,1.0,1.0>;
integer COLON=3;  //face colon chrono

//Links
integer CENTERLINE=0;
integer LEFTLINE=0;
integer RIGHTLINE=0;
integer CHRONO=0;

list tmp;
string name;

integer winnerFlag=FALSE;
integer runnerupFlag=FALSE;
key ownerKey;
string ownerName;

integer listenHandle;
integer listenTime;
key listenKey;

integer start_at;           // The point in time (as unix time) when the start is planned / has been. -1 if not active
integer last_tick;          // The last second (Unix time) the counter worked

list cmd_list;
string cmd;
key texarrow;

//timer temporals vars
integer now_timer;
//END Global Variables/////////////////////////////////////////////

//Blocks/////////////////////////////////////////////
//Notecard Block->
//This block of functions reads a notecard

readfirstNCLine(string name) 
// Start reading a notecard line. Only call this when this notecard exists
{
    currentLine=0; 
    currentName=name;
    if (llGetInventoryType("settings")!=INVENTORY_NOTECARD){
        llSay(0, "Unable to find the 'settings' notecard. Default settings will be loaded.");
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

    dbgSay("Settings Set: "+pText);

    string keystring=llList2String(parts, 0);
    keystring=llToLower(llStringTrim(keystring, STRING_TRIM));
    string value=llList2String(parts, 1);
    value=llStringTrim(value, STRING_TRIM);
    
    if (value=="") return;

    if (keystring=="channel") {
        if ((integer)value!=0) channel=(integer)value;
        return;
    }
    if (keystring=="pingchannel") {
        if ((integer)value!=0) pingChannel=(integer)value;
        return;
    }
    if (keystring=="owner") {
        owners+=value;
        return;
    }
    if (keystring=="displayname") {
        setDName=(integer)value;
        return;
    }
    if (keystring=="autolock") {
        autolock=(integer)value;
        return;
    }
    if (keystring=="autounlock") {
        autounlock=(integer)value;
        return;
    }
    if (keystring=="autolocktime") {
        lockTimeDef=60*(integer)value;
        return;
    }
    if (keystring=="alphaline") {
        alphaline=(float)value;
        return;
    }
    if (keystring=="visibleleft") {
        visibleleft=(integer)value;
        return;
    }
    if (keystring=="visibleright") {
        visibleright=(integer)value;
        return;
    }
    if (keystring=="autolocktype") {
        autolockType=(integer)value;
        return;
    }
    if (keystring=="region_relay_channel") {
        region_relay_channel=(integer)value;
        return;
    }
    if (keystring=="startsay") {
        startsay=(integer)value;
        return;
    }
    if (keystring=="menuoutregion") {
        menuoutregion=(integer)value;
        return;
    }
    if (keystring=="laptimessay") {
        laptimessay=(integer)value;
        return;
    }
    if (llGetSubString(keystring,0,4)=="color") {
        if (keystring=="color-set-chrono") {
            colorchrono=(integer)value;
            return;
        }
        if (keystring=="color-set-line") {
            colorline=(integer)value;
            return;
        }
        if (keystring=="color+60") {
            color61=(vector)value;
            return;
        }
        if (keystring=="color-60") {
            color60=(vector)value;
            return;
        }
        if (keystring=="color-30") {
            color30=(vector)value;
            return;
        }
        if (keystring=="color-10") {
            color10=(vector)value;
            return;
        }
        if (keystring=="color-start") {
            colorstart=(vector)value;
            return;
        }
        if (keystring=="color-normal") {
            colornormal=(vector)value;
            return;
        }
    }
    if (keystring=="dbg") {
        dbg=(integer)value;
        return;
    }
    llSay(0, "Settings: Unknown keyword: \""+keystring+"\"");    
}

//<-Notecard Block
getLinkNums() 
{
    integer i;
    integer linkcount=llGetNumberOfPrims();  
    if (model==162) CENTERLINE=1;
    for (i=1;i<=linkcount;++i) {
        string str=llGetLinkName(i);
        if (str=="leftline") 
            LEFTLINE=i;
        else if (str=="rightline") 
            RIGHTLINE=i;
        else if (str=="chrono") 
            CHRONO=i;
    }
} 

dbgSay(string text)
{
    if (dbg>0) llSay(0, text);
}

DisplayTime(integer sec_to_start)   
// Update the line's appearance and shouts out verbal messages
{
    integer sw=0;
    vector Color;
    string msg="";
    if (sec_to_start<-120) {
        sw=1;
        Color=color61;
    } else if (sec_to_start==-120) {
        msg="TWO minutes to the start"; 
        sw=1;
        Color=color61;
    } else if (sec_to_start==-60) {
        msg="ONE minute to the start"; 
        sw=1;
        Color=color60;
    } else if (sec_to_start==-30) {
        msg="30 SECONDS to the start"; 
        sw=1;
        Color=color30;
    } else if (sec_to_start==-20) {
        msg="20 SECONDS to the start"; 
    } else if (sec_to_start==-15) {
        msg="15 SECONDS to the start"; 
    } else if ( (sec_to_start>=-10) && (sec_to_start<0) ) {
        msg=(string)(-sec_to_start)+" SECONDS to the start"; 
        if(sec_to_start==-10){
            sw=1;
            Color=color10;
        }
    } else if (sec_to_start>=0) {
        llPlaySound("w1hs",1);
        msg="RACE STARTED"; 
        llShout(channel,"starr|");
        sw=1;
        Color=colorstart;
        last_tick=0; //stop timer for display timer
    }
    if(msg!=""){
        llShout(0,"/me : "+msg); 
        if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
    }
    if (sw==1 && colorline==1) {
        llRegionSay(pingChannel,"colorext|"+(string)Color);
        if (visible) {
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,0,Color,alphaline,PRIM_COLOR,1,Color,alphaleft]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,0,Color,alphaline,PRIM_COLOR,1,Color,alpharight]);
            if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,0,Color,alphaline,PRIM_COLOR,1,Color,0.0]);
        }else{
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,0,Color,0.0,PRIM_COLOR,1,Color,0.0]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,0,Color,0.0,PRIM_COLOR,1,Color,0.0]);
            if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,0,Color,0.0,PRIM_COLOR,1,Color,0.0]);
        }
    }
}

restart(key id)
{
    if(id){
        string s;
        if (start_at>=0){
            s="The race is over, timer is stopped.";
            llShout(channel,"stopr|");
        } else {
            s="Ready for race.";
        }
        if (id!=NULL_KEY) llShout(0,"/me : "+s);
        if(region_relay_channel!=0) llRegionSay(region_relay_channel, s);
    }
    start_at=-1;
    last_tick=0;
    if (visible) {
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,alphaleft]);
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,alpharight]);
        if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,0.0]);
    }
}

start(integer p)
{
    start_at=p;
    if(startsay==0 || startsay==2) llRegionSay(channel, "start,"+(string)start_at);
    if(startsay==1 || startsay==2) llShout(channel, "start,"+(string)start_at);
    string msg="The race start, timer is running.";
    llShout(0,"/me : "+msg); 
    if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
    DisplayTime(llGetUnixTime()-start_at);
    last_tick=llGetUnixTime();
    llSetTimerEvent(0.5);
}

setVisible(integer pvisible, integer pways)
{
    visible=pvisible;
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_COLOR,0]);
    vector c=llList2Vector(l,0);
    float f=.0;
    if (pways!=0) f=1.0;
    if (visible) {
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,0,c,alphaline,PRIM_COLOR,1,c,alphaleft,PRIM_COLOR,2,COLOR_WHITE,f]); 
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,0,c,alphaline,PRIM_COLOR,1,c,alpharight,PRIM_COLOR,2,COLOR_WHITE,f]); 
        if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,0,c,alphaline,PRIM_COLOR,1,c,0.0,PRIM_COLOR,2,COLOR_WHITE,f]); 
        llSetLinkAlpha(CHRONO,1.0,ALL_SIDES);
    } else {
        if (LEFTLINE>0) llSetLinkAlpha(LEFTLINE,0.0,ALL_SIDES);
        if (RIGHTLINE>0) llSetLinkAlpha(RIGHTLINE,0.0,ALL_SIDES);
        if (CENTERLINE>0) llSetLinkAlpha(CENTERLINE,0.0,ALL_SIDES);
        llSetLinkAlpha(CHRONO,0.0,ALL_SIDES);
    }    
}

setWays(integer ps)
{
    list l;
    float f;
    if(ps==1) f=0.0;
    else if(ps==-1) f=PI;
    else { 
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,2,COLOR_WHITE,0.0]); 
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,2,COLOR_WHITE,0.0]);  
        if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,2,COLOR_WHITE,0.0]);  
    }
    if (ps!=0) {
        if (texarrow) {
            if (LEFTLINE>0) { 
                l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_TEXTURE,2]);
                llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,2,COLOR_WHITE,visible,PRIM_TEXTURE,2,texarrow,llList2Vector(l,1),llList2Vector(l,2),f]);
            }
            if (RIGHTLINE>0) {         
                l=llGetLinkPrimitiveParams(RIGHTLINE,[PRIM_TEXTURE,2]);
                llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,2,COLOR_WHITE,visible,PRIM_TEXTURE,2,texarrow,llList2Vector(l,1),llList2Vector(l,2),f]);
            }
            if (CENTERLINE>0) {         
                l=llGetLinkPrimitiveParams(CENTERLINE,[PRIM_TEXTURE,2]);
                llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,2,COLOR_WHITE,visible,PRIM_TEXTURE,2,texarrow,llList2Vector(l,1),llList2Vector(l,2),f]);
            }
        }
    }        
    llRegionSay(pingChannel,"waysext|"+(string)ps); 
}

inicio()
{
    getTextures();
    string s="config|"+(string)channel+"|"+(string)pingChannel+"|"+(string)setDName+"|"+(string)autolock+"|"+(string)lockTimeDef+"|"+(string)autolockType+"|"+(string)alphaline+"|"+(string)colornormal+"|"+(string)dbg+"|"+(string)region_relay_channel+"|"+(string)autounlock+"|"+(string)menuoutregion+"|"+(string)laptimessay;
    integer n=llGetListLength(owners);
    string ss="";
    while (n>0) {
        if (ss!="") ss+="#";
        ss+=llList2String(owners,n-1);
        n--;
    }
    llMessageLinked(LINK_THIS, 0, s, ss);  //send note options
    owners=[];
    if (visibleleft==1) alphaleft=alphaline;
    else alphaleft=0.0;
    if (visibleright==1) alpharight=alphaline;
    else alpharight=0.0;
    if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,alphaleft,PRIM_COLOR,2,COLOR_WHITE,1.0,PRIM_COLOR,3,COLOR_WHITE,0.0]); 
    if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,alpharight,PRIM_COLOR,2,COLOR_WHITE,1.0,PRIM_COLOR,3,COLOR_WHITE,0.0]); 
    if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(CENTERLINE,[PRIM_COLOR,0,colornormal,alphaline,PRIM_COLOR,1,colornormal,0.0,PRIM_COLOR,2,COLOR_WHITE,1.0]); 

    dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    
    llSleep(2);
    s="|"+(string)colorchrono+"|"+(string)color61+"|"+(string)color60+"|"+(string)color30+"|"+(string)color10+"|"+(string)colorstart+"|"+(string)colornormal;
    llMessageLinked(LINK_ALL_CHILDREN, 0, "colors"+s, NULL_KEY);  //send colors to chrono
    llRegionSay(pingChannel,"colorsline"+s);
}

updateTextures(string t1, string t2, string t3, string t4, string ls, string as)  //Title, Clock numbers, Line, Arrows, line scale, arrow scale   Textures
{
    string s2="";
    string s4="";  
    integer nls=1;
    integer nas=1;
    t1=llStringTrim(t1, STRING_TRIM); 
    t2=llStringTrim(t2, STRING_TRIM); 
    t3=llStringTrim(t3, STRING_TRIM); 
    t4=llStringTrim(t4, STRING_TRIM); 
    key k;
    if (t1!="") {
        k=(key)t1;
        if (k) llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,0,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
    }
    if (t2!="") { 
        k=(key)t2;
        if (k) {
            s2=t2;
            llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,1,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,2,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,3,k,<1.0,1.0,0.0>,<0.74,0.0,0.0>,0.0,PRIM_TEXTURE,4,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,5,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,6,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,7,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);  
        }
    }
    if (t3!="") {
        k=(key)t3;
        if (k) {
            nls=(integer)ls;
            if(nls<1 || nls>20) nls=1;
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_TEXTURE,0,k,<nls,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,1,k,<nls,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,0,k,<nls,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,1,k,<nls,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
            if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,0,k,<nls,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
        }
    }
    if (t4!="") {
        k=(key)t4;
        if (k) {
            nas=(integer)as;
            if(nas<1 || nas>20) nas=1;
            s4=t4;
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_TEXTURE,2,k,<nas,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,2,k,<nas,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
            if (CENTERLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,2,k,<nas,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
        }
    }
    if (s2!="" || s4!="") {
        list li=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
        string desc=llList2String(li,0);
        li=llParseString2List(desc,["#"],[]);
        integer n=llGetListLength(li);
        integer i;
        string t1="";
        string t2="";
        for (i=0;i<n;i++) {
            if (llGetSubString(llList2String(li,i),0,1)=="t1") t1=llList2String(li,i);
            if (llGetSubString(llList2String(li,i),0,1)=="t2") t2=llList2String(li,i);
        }
        if (s2!="") t1="t1="+s2;        
        if (s4!="") t2="t2="+s4;        
        n=llSubStringIndex(desc,"#t");
        string desc0;
        if(n>0) desc0=llGetSubString(desc,0,n-1);
        else if(n==0) desc0="";
        else desc0=desc;
        desc=desc0+"#"+t1+"#"+t2;
        llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,desc]);
        if (s2!="") llMessageLinked(LINK_ALL_CHILDREN, 0, "updtextchrono|"+s2, NULL_KEY);
        if (s4!="") texarrow=(key)s4;
    }
}

getTextures()
{
        list l=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
        string desc=llList2String(l,0);
        l=llParseString2List(desc,["#"],[]);
        integer n=llGetListLength(l);
        integer i;
        string t1=NULL_KEY;
        string t2=NULL_KEY;
        for (i=0;i<n;i++) {
            if (llGetSubString(llList2String(l,i),0,1)=="t1") t1=(key)llGetSubString(llList2String(l,i),3,-1);
            if (llGetSubString(llList2String(l,i),0,1)=="t2") t2=(key)llGetSubString(llList2String(l,i),3,-1);
        }
        l=llGetLinkPrimitiveParams(CHRONO,[PRIM_TEXTURE,COLON]);
        key k=llList2Key(l,0);
        if (k) t1=k;
        else if (t1) t1=t1;
        else llSay(0,"Display numbers texture is not available, load default textures");

        l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_TEXTURE,2]);
        k=llList2Key(l,0);
        if (k) t2=k;
        else if (t2) t2=t2;
        else llSay(0,"Arrow texture is not available, load default textures");
        
        n=llSubStringIndex(desc,"#t");
        string desc0;
        if(n>0) desc0=llGetSubString(desc,0,n-1);
        else if(n==0) desc0="";
        else desc0=desc;
        desc=desc0+"#t1="+(string)t1+"#t2="+(string)t2;
        llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,desc]);

        if (t1) llMessageLinked(LINK_ALL_CHILDREN, 0, "updtextchrono|"+(string)t1, NULL_KEY);
        if (t2) texarrow=t2;
}

default 
{
    state_entry()
    {
        getLinkNums();
        listenHandle=0;
        readfirstNCLine("settings");
    }
    
    listen(integer pchannel, string name, key id, string cmd) 
    {
        if (listenKey!=NULL_KEY && listenKey==llGetOwnerKey(id)) {
            list l=llParseString2List(cmd,["|"],[""]);
            if (llList2String(l,0)=="updtextans") {
                dbgSay("receive update texture answer");
                if(llGetListLength(l)<7) llRegionSayTo(listenKey,0,"Designer Kit wrong configuration");
                else { 
                    updateTextures(llList2String(l,1),llList2String(l,2),llList2String(l,3),llList2String(l,4),llList2String(l,5),llList2String(l,6));
                    llRegionSay(pingChannel,"updtextext|"+llList2String(l,1)+"|"+llList2String(l,2)+"|"+llList2String(l,3)+"|"+llList2String(l,4)+"|"+llList2String(l,5)+"|"+llList2String(l,6));
                    llRegionSayTo(listenKey,0,"Updated textures");
                }
                llListenRemove(listenHandle);
                listenHandle=0;
                listenTime=0;
                listenKey=NULL_KEY;
            }
        }
    }
    
    link_message(integer sender_num, integer num, string str, key id)
    {
        if (str=="start") {
            start(num);
            return;
        }        
        if (str=="restart") {
            restart(id);
            return;
        }        
        if (str=="visible") {
            if (num==0) setVisible(0,0);
            else setVisible(1,num-10);
            return;
        }  
        if (llGetSubString(str,0,5)=="setopt") {
            list parse=llParseString2List(str, ["|"], []);
            setWays((integer)llList2String(parse,3));
        }
        if (str=="Load Texture") {
            listenHandle = llListen(updtextChannel, "", "", "");
            llRegionSayTo(id,0,"Looking for texture updater");
            listenKey=id;
            dbgSay("send update texture request");
            llRegionSay(updtextChannel,"updtextreq|"+(string)id);
            listenTime=llGetUnixTime()+30;
            if (last_tick==0) llSetTimerEvent(31);
            return;
        }        
    }
        
    dataserver( key queryid, string data ) 
    {
        if (queryid == requestInFlight){
            if (data == EOF){
                requestInFlight = NULL_KEY;
                inicio();
            }else{
                readNextNCLine(data);
            }
        }        
    }


    timer()
    {
        now_timer=llGetUnixTime();
        if (last_tick>0) {
            if (now_timer==last_tick) return;
            last_tick=now_timer;
            DisplayTime(now_timer-start_at);  //negative during prestart
        }
        if (listenHandle>0 && listenTime<now_timer) {
            llListenRemove(listenHandle);
            listenHandle=0;
            listenTime=0;
            listenKey=NULL_KEY;
        }
        if (last_tick==0 && listenHandle==0){ 
            //dbgSay("close timer event Aux ");
            llSetTimerEvent(0.0); 
        }
    }
}