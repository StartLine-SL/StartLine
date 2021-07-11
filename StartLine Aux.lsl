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
integer model=0;    //0 normal model   1 fly model
integer updtextChannel=-56784325;
integer channel=-8001;       //The channel to listen for start, reset and miscellanous other commands for nav huds
integer pingChannel=-2256845;   //the channel to listen for chronometers and others objects 
integer flyChannel=-51234567;
integer region_relay_channel=0;  //this is where the line will relay information normally said in chat Default is 0 No relay information
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
list netlinkN;
list netlinkK;
integer visibleleft=1;
integer visibleright=1;
float alphaleft;
float alpharight;
integer startsay;
integer menuoutregion=0;
integer autounlock=1;
integer startN;

integer visible=1;
integer nMenuTime;
string  lockName="";
key  lockID=NULL_KEY;
integer locktime=-1; //time (seconds) the lockout ends  -1 no block
integer lockType=0;  //Block type  0 no block   1 soft block   2 hard block
key unlockID=NULL_KEY;
integer menutimeout=120;
integer ways;
integer stage=-1;

integer hr;
integer mn;
integer sc;
string hms;

//constants
vector COLOR_WHITE=<1.0,1.0,1.0>;
integer SIGN=1;  //face sign chrono
integer SUBTIT=7;  //face subtitulo chrono

//Links
integer LEFTLINE=0;
integer RIGHTLINE=0;
integer LEFTARROW=0;
integer RIGHTARROW=0;
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
integer set_time;
integer lineStatus;
integer startat1;
integer startat2;
integer startat3;
integer startat4;
integer swDispWays;

list cmd_list;
string cmd;
key texarrow;

integer lineMode;
integer raceTime;
integer time1;
integer time2;
integer time3;

//timer temporals vars
integer now_timer;

//fly model variables
key titletexture=NULL_KEY;
key numberstexture=NULL_KEY;
key linetexture=NULL_KEY;
key arrowtexture=NULL_KEY;
key modetexture=NULL_KEY;
key redbuoytexture=NULL_KEY;
key redbuoytextureupdown=NULL_KEY;
key greenbuoytexture=NULL_KEY;
key greenbuoytextureupdown=NULL_KEY;
key buoylogotexture=NULL_KEY;
integer linescale;
integer arrowscale;

vector flyposition;
integer flyreztime;
string flysimname;
float flychronoheight=9;
integer flylinerotation;
integer flydepth=-4;
integer flybuoys=1;

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
        if(model==1){
            llSay(0, "Unable to find the '"+name+"' notecard.");
        }else{
            llSay(0, "Unable to find the 'settings' notecard. Default settings will be loaded.");
            inicio();
        }
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
    if (keystring=="flyChannel") {
        flyChannel=(integer)value; 
        return;
    }
    if (keystring=="admin") {
        owners+=value;
        return;
    }
    if (keystring=="netlink") { 
        integer n=llSubStringIndex(value,",");
        if (n>0) {
            if (llGetListLength(netlinkN)<10) {
                string sn=llStringTrim(llGetSubString(value,0,n-1), STRING_TRIM);
                if (sn!="") {
                    if (llStringLength(sn)>8) sn=llStringTrim(llGetSubString(sn,0,7), STRING_TRIM);
                    string sk=llStringTrim(llGetSubString(value,n+1,-1), STRING_TRIM);
                    if(llStringLength(sk)==32){
                        sk=llGetSubString(sk,0,7)+"-"+llGetSubString(sk,8,11)+"-"+llGetSubString(sk,12,15)+"-"+llGetSubString(sk,16,19)+"-"+llGetSubString(sk,20,31);
                        if ((key)sk) {
                            netlinkN+="NL-"+sn;
                            netlinkK+=sk;
                        }
                    }
                }
            }
        }
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
    if (keystring=="menutimeout") {
        menutimeout=(integer)value;
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
    if(model==1){
        if (keystring=="titletexture") titletexture=(key)value;
        else if (keystring=="numberstexture") numberstexture=(key)value;
        else if (keystring=="linetexture") linetexture=(key)value;
        else if (keystring=="arrowtexture") arrowtexture=(key)value;
        else if (keystring=="modetexture") modetexture=(key)value;
        else if (keystring=="linescale") linescale=(integer)value;
        else if (keystring=="arrowscale") arrowscale=(integer)value;
        
        else if (keystring=="flyposition") flyposition=(vector)value;
        else if (keystring=="flyreztime") flyreztime=(integer)value;
        else if (keystring=="flysimname") flysimname=value;
        else if (keystring=="flychronoheight") flychronoheight=(float)value;
        else if (keystring=="flylinerotation") flylinerotation=(integer)value;
        else if (keystring=="flydepth") flydepth=(integer)value;
    
        else if (keystring=="redbuoytexture") redbuoytexture=value;
        else if (keystring=="redbuoytextureupdown") redbuoytextureupdown=value;
        else if (keystring=="greenbuoytexture") greenbuoytexture=value;
        else if (keystring=="greenbuoytextureupdown") greenbuoytextureupdown=value;
        else if (keystring=="buoylogotexture") buoylogotexture=value;
    }
    
    llSay(0, "Settings: Unknown keyword: \""+keystring+"\"");    
}

//<-Notecard Block 
getLinkNums() 
{
    integer i;
    integer linkcount=llGetNumberOfPrims();  
    for (i=1;i<=linkcount;++i) {
        string str=llGetLinkName(i);
        if (str=="leftline") 
            LEFTLINE=i;
        else if (str=="rightline") 
            RIGHTLINE=i;
        else if (str=="leftarrow") 
            LEFTARROW=i;
        else if (str=="rightarrow") 
            RIGHTARROW=i;
        else if (str=="chrono") 
            CHRONO=i;
    }
} 

dbgSay(string text)
{
    if(dbg!=0){ 
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
}

string sec2hms(float seconds) 
{
    integer hr;
    integer mn;
    integer sc;
    string hms;
    if (seconds<0) hms="-";
    else hms="";
    seconds=llFabs(seconds);
    hr=llFloor(seconds/3600.);
    mn=llFloor((seconds-hr*3600)/60);
    sc=llRound(seconds-mn*60-hr*3600);
    if(hr>0) hms+=(string)hr+"h ";
    if(mn>0 || (hr>0 && sc>0)) hms+=(string)mn+"m ";
    if(sc>0) hms+=(string)sc+"s ";
    return hms;
}

DisplayTime(integer sec_to_start)   
// Update the line's appearance and shouts out verbal messages
{
    integer sw=0;
    vector Color;
    string msg="";
    string msg2="";
    if(lineMode==2 || lineMode==7){
        if (sec_to_start>120){ 
            sw=1;
            Color=colorstart;
        }else if (sec_to_start==120){ 
            msg="TWO minutes left"; 
            sw=1;
            Color=color60;
        }else if (sec_to_start==60){ 
            msg="ONE minute left"; 
            sw=1;
            Color=color30;
        }else if (sec_to_start==30){
            msg="30 SECONDS left"; 
            sw=1;
            Color=color10;
        }else if(sec_to_start<0){ 
            llPlaySound("w1hs",1);
            msg="RACE FINISH"; 
            //llShout(channel,"starr|"); //*****nofly
            sw=1;
            Color=color61;
            last_tick=0; //stop timer for display timer
        }
    }else{
        if(sec_to_start<-120){
            if(lineMode!=1){
                sw=1;
                Color=color61;
            }
        }else if (sec_to_start==-120) {
            msg="TWO minutes to the start"; 
            sw=1;
            if(lineMode!=1) Color=color61;
        }else if (sec_to_start==-60) {
            msg="ONE minute to the start"; 
            sw=1;
            if(lineMode!=1) Color=color60;
        }else if (sec_to_start==-30) {
            msg="30 SECONDS to the start"; 
            sw=1;
            if(lineMode!=1) Color=color30;
        }else if (sec_to_start==-20) {
            msg="20 SECONDS to the start"; 
        }else if (sec_to_start==-15) {
            msg="15 SECONDS to the start"; 
        }else if ( (sec_to_start>=-10) && (sec_to_start<0) ) {
            msg=(string)(-sec_to_start)+" SECONDS to the start"; 
            if(sec_to_start==-10){
                sw=1;
                Color=color10;
            }
        }else if (sec_to_start>=0) {
            llPlaySound("w1hs",1);
            msg="RACE STARTED"; 
            llShout(channel,"starr|"); //*****nofly
            sw=1;
            Color=colorstart;
            last_tick=0; //stop timer for display timer
            if(lineMode==3) llMessageLinked(LINK_THIS,1000+startN,"raceend",""); 
            else if(lineMode==6) llMessageLinked(LINK_THIS,1000,"netstart","");   //to startline script
        }
    }
    if(lineMode==1){
        if(sec_to_start<-time3){
            if(stage<0){
                stage=0;
                msg2="Boats can not enter the UPPER H. Stage 1 time: "+sec2hms(-time3);
                sw=1;
                Color=color61;
            }
        }else if(sec_to_start==-time3 && time3>time2){
            stage=1;
            msg2="Boats may enter the UPPER H without PENALTY. Stage 2 time: "+sec2hms(-time2);
            sw=1;
            Color=color60;
        }else if(sec_to_start==-time2 && time2>time1){
            stage=2;
            msg2="Boats may enter the UPPER H with a PENALTY. Stage 3 time: "+sec2hms(-time1);
            sw=1;
            Color=color30;
        }else if(sec_to_start==-time1){
            stage=3;
            msg2="Boats can not enter the UPPER H.";
            sw=1;
            Color=color30;
        }
        if(msg2!=""){
            llMessageLinked(LINK_ALL_OTHERS,5000+stage,"stage",""); 
            msg2="STAGE "+(string)stage+": "+msg2;
            if(msg!="") msg=msg2+"\n"+msg;
            else msg=msg2;
        }
    }        
    
    if(msg!=""){
        llMessageLinked(LINK_ALL_OTHERS,6000,"sendmsg2",msg);  
    }
    if (sw==1 && colorline==1) {
        llRegionSay(pingChannel,"colorext|"+(string)Color);
        if (visible) {
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,Color,alphaline,PRIM_COLOR,2,Color,0.0]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,Color,alphaline,PRIM_COLOR,4,Color,0.0]);
        }else{
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,Color,0.0]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,Color,0.0]);
        }
    }
}

putColor(integer sec_to_start)   
// Update the line's appearance only for network remote start countdown
{
    vector Color=ZERO_VECTOR;
    if(sec_to_start<-120) Color=color61;
    else if (sec_to_start<-60) Color=color61;
    else if (sec_to_start<-30) Color=color60;
    else if (sec_to_start<-10) Color=color30;
    else if(sec_to_start<0) Color=color10;
    else if (sec_to_start>=0) Color=colorstart;
    if (Color!=ZERO_VECTOR && colorline==1) {
        if (visible) {
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,Color,alphaline,PRIM_COLOR,2,Color,0.0]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,Color,alphaline,PRIM_COLOR,4,Color,0.0]);
        }else{
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,Color,0.0]);
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,Color,0.0]);
        }
    }
}

restart(key id)
{
    if(id){ 
        if(model!=1){
            integer x;
            if (start_at>=0){
                x=1;
                llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","stopr|");
                //llShout(channel,"stopr|");
            }
            llMessageLinked(LINK_ALL_OTHERS,6001+x,"sendmsg2","");  //x=0 Ready for race.   x=1 The race is over, timer is stopped.
            //if (id!=NULL_KEY) llShout(0,"/me : "+s);
            //if(region_relay_channel!=0) llRegionSay(region_relay_channel, s);
        }
        lineStatus=0;
    }
    start_at=-1;
    last_tick=0;
    if(lineMode!=5) lineStatus=0;
    swDispWays=0;
    stage=-1;
    //llMessageLinked(LINK_THIS,stage,"stage",""); 
    if(lineMode==1) dispWays(1);
    if (visible) {
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,2,colornormal,0.0]);
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,4,colornormal,0.0]);
    }
}

start(integer p)
{
    
    if(lineMode==0){
        lineStatus=0;
        start_at=p;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        llMessageLinked(LINK_ALL_OTHERS,6004,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }else if(lineMode==1){
        lineStatus=0;
        start_at=p;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        llMessageLinked(LINK_ALL_OTHERS,6004,"sendmsg2","");
        swDispWays=1;
        stage=-1;
        dispWays(1);
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        //llMessageLinked(LINK_THIS,stage,"stage",""); 
        llSetTimerEvent(0.5);
    }else if(lineMode==2 || lineMode==7){
        lineStatus=0;
        start_at=p;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        if (lineMode==2) llMessageLinked(LINK_ALL_OTHERS,6007,"sendmsg2","");
        else llMessageLinked(LINK_ALL_OTHERS,6009,"sendmsg2","");
        DisplayTime(start_at-llGetUnixTime());
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }else if(lineMode==3){ //multistart
        lineStatus=0;
        start_at=p;
        if(startN==1){
            startat1=start_at;
            startat2=0;
            startat3=0;
            startat4=0;
        }else if(startN==2) startat2=start_at;
        else if(startN==3) startat3=start_at;
        else if(startN==4) startat4=start_at;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        llMessageLinked(LINK_ALL_OTHERS,6008,"sendmsg2",(string)startN);  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }else if(lineMode==4){
        lineStatus=0;
        start_at=p;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        llMessageLinked(LINK_ALL_OTHERS,6006,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=0;
        //llSetTimerEvent(0.5);
    }else if(lineMode==5){
        llMessageLinked(LINK_ALL_OTHERS,6005,"sendmsg2","");  ///me : Countdown start, timer is running. shout channel 0 and relay channel
        lineStatus=2;
        last_tick=llGetUnixTime();
        start_at=last_tick+set_time;
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline|"+(string)start_at);
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        DisplayTime(last_tick-start_at);
        llSetTimerEvent(0.5);
    }else if(lineMode==6){
        lineStatus=0;
        start_at=p;
        putColor(llGetUnixTime()-start_at);   
        if(startsay==0 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6001,"chanmsg","start,"+(string)start_at);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_ALL_OTHERS,6000,"chanmsg","start,"+(string)start_at);   //shout to channel
        llMessageLinked(LINK_ALL_OTHERS,6004,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }
}

prestart()
{
    if(lineMode==5){
        llMessageLinked(LINK_SET, 0, "restart", (string)NULL_KEY+","+(string)(-set_time));
        llRegionSay(pingChannel,"restartline|"+(string)(-set_time));
        lineStatus=1;
        llSetTimerEvent(30.0);
        llMessageLinked(LINK_ALL_OTHERS,6003,"sendmsg2","");   //30 seconds to start the countdown
    }
}
/*
setVisible(integer pvisible, integer pways)
{
    visible=pvisible;
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_COLOR,0]);
    vector c=llList2Vector(l,0);
    l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_COLOR,0]);
    vector a=llList2Vector(l,0);
    float f=0.0;
    if (pways!=0) f=1.0;
    if (visible) {
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,c,alphaline,PRIM_COLOR,2,c,0.0]); 
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,c,alphaline,PRIM_COLOR,4,c,0.0]); 
        if (LEFTARROW>0) llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_COLOR,ALL_SIDES,a,f,PRIM_COLOR,2,a,0.0]); 
        if (RIGHTARROW>0) llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_COLOR,ALL_SIDES,a,f,PRIM_COLOR,4,a,0.0]); 
        llSetLinkAlpha(CHRONO,1.0,ALL_SIDES);
    } else {
        if (LEFTLINE>0) llSetLinkAlpha(LEFTLINE,0.0,ALL_SIDES);
        if (RIGHTLINE>0) llSetLinkAlpha(RIGHTLINE,0.0,ALL_SIDES);
        if (LEFTARROW>0) llSetLinkAlpha(LEFTARROW,0.0,ALL_SIDES); 
        if (RIGHTARROW>0) llSetLinkAlpha(RIGHTARROW,0.0,ALL_SIDES); 
        llSetLinkAlpha(CHRONO,0.0,ALL_SIDES);
    }    
}
*/
setWays(integer ps)  //-1=-Y  1=Y  0=both
{
    if(ways==ps) return;
    list l;
    float f;
    if(ps==1) f=0.0;
    else if(ps==-1) f=PI;
    else { 
        if (LEFTARROW>0) llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_COLOR,0,COLOR_WHITE,0.0]); 
        if (RIGHTARROW>0) llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_COLOR,0,COLOR_WHITE,0.0]);  
    }
    if (ps!=0) {
        if (texarrow) {
            if (LEFTARROW>0) { 
                l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_TEXTURE,0]);
                llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_COLOR,0,COLOR_WHITE,visible,PRIM_TEXTURE,0,texarrow,llList2Vector(l,1),llList2Vector(l,2),f]);
            }
            if (RIGHTARROW>0) {         
                l=llGetLinkPrimitiveParams(RIGHTARROW,[PRIM_TEXTURE,0]);
                llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_COLOR,0,COLOR_WHITE,visible,PRIM_TEXTURE,0,texarrow,llList2Vector(l,1),llList2Vector(l,2),f]);
            }
        }
    }  
    ways=ps;      
    llRegionSay(pingChannel,"waysext|"+(string)ps); 
}

dispWays(integer ps)  //ps=1 or 0  display or no display direction arrow according ways value
{
    if(ps){
        if(ways==0) ps=0;
        else ps=visible;
    }
    if (LEFTARROW>0) llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_COLOR,0,COLOR_WHITE,ps]); 
    if (RIGHTARROW>0) llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_COLOR,0,COLOR_WHITE,ps]);  
}

inicio()
{
    string s;
    string ss;
    getTextures();
    if(model==1){
        s=(string)flyChannel+"|"+(string)flyposition+"|"+(string)flyreztime+"|"+flysimname+"|"+(string)flychronoheight+"|"+(string)flylinerotation+"|"+(string)flydepth;
        s+="#"+(string)colorchrono+"|"+(string)color61+"|"+(string)color60+"|"+(string)color30+"|"+(string)color10+"|"+(string)colorstart+"|"+(string)colornormal;
        llMessageLinked(LINK_THIS, 0, "flyconfig", s);
        s=(string)titletexture+"|"+(string)numberstexture+"|"+(string)modetexture+"|"+(string)linetexture+"|"+(string)arrowtexture+"|"+(string)linescale+"|"+(string)arrowscale+"|"+(string)redbuoytexture+"|"+(string)redbuoytextureupdown+"|"+(string)greenbuoytexture+"|"+(string)greenbuoytextureupdown+"|"+(string)buoylogotexture;
        llMessageLinked(LINK_THIS, 0, "flytexture", s);
    }
    s=(string)channel+","+(string)pingChannel+","+(string)setDName+","+(string)autolock+","+(string)lockTimeDef+","+(string)autolockType+","+(string)alphaline+","+(string)colornormal+","+(string)dbg+","+(string)region_relay_channel+","+(string)autounlock+","+(string)menuoutregion+",1,"+(string)menutimeout+","+(string)model+","+(string)flyChannel;
    /*
    integer n=llGetListLength(owners);
    string ss="";
    while (n>0) {
        if (ss!="") ss+=",";
        ss+=llList2String(owners,n-1);
        n--;
    }
    */
    llMessageLinked(LINK_SET, 0, "config", s);  //send note options
    if(llGetListLength(owners)>0){
        ss=llList2CSV(owners);
        owners=[];
        if(ss!="") llMessageLinked(LINK_ALL_OTHERS, 4000, "owners", ss);  //send owners
    }
    if(llGetListLength(netlinkN)>0){
        s=llList2CSV(netlinkN);
        netlinkN=[];
        ss=llList2CSV(netlinkK);
        netlinkK=[];
        llMessageLinked(LINK_THIS, 4000, "netlinkn", s);  //send netlink names
        llMessageLinked(LINK_THIS, 4000, "netlinkk", ss);  //send netlink keys
    }

    if (visibleleft==1) alphaleft=alphaline;
    else alphaleft=0.0;
    if (visibleright==1) alpharight=alphaline;
    else alpharight=0.0;
    if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,2,colornormal,0.0]); 
    if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,4,colornormal,0.0]); 

    dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    
    llSleep(2);
    s="|"+(string)colorchrono+"|"+(string)color61+"|"+(string)color60+"|"+(string)color30+"|"+(string)color10+"|"+(string)colorstart+"|"+(string)colornormal;
    llMessageLinked(LINK_ALL_CHILDREN, 0, "colors"+s, NULL_KEY);  //send colors to chrono
    llRegionSay(pingChannel,"colorsline"+s);
}

updateTextures(string t1, string t2, string t3, string t4, string t5, string ls, string as)  //Title, Clock numbers, mode, Line, Arrows, line scale, arrow scale   Textures
{
    string s2="";
    string s3="";
    string s5="";  
    integer nls=1;
    integer nas=1;
    t1=llStringTrim(t1, STRING_TRIM); //title
    t2=llStringTrim(t2, STRING_TRIM); //numbers 
    t3=llStringTrim(t3, STRING_TRIM); //subtitle
    t4=llStringTrim(t4, STRING_TRIM); //line 
    t5=llStringTrim(t5, STRING_TRIM); //arrows 
    key k;
    if (t1!="") {
        k=(key)t1;
        if (k) llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,0,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
    }
    if (t2!="") { 
        k=(key)t2;
        if (k) {
            s2=t2;
            llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,1,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,2,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,3,k,<1.0,1.0,0.0>,<0.74,0.0,0.0>,0.0,PRIM_TEXTURE,4,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,5,k,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,6,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
        //,PRIM_TEXTURE,7,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);  
        }
    }
    if (t3!="") {
        k=(key)t3;
        if (k){ 
            s3=t3;
            llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,7,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);   
        } 
    }
    if (t4!="") {
        k=(key)t4;
        if (k) {
            nls=(integer)ls;
            if(nls<1 || nls>20) nls=1;
            if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_TEXTURE,0,k,<nls,0.33333,0.0>,<0.0,0.16800,0.0>,0.0,PRIM_TEXTURE,1,k,<nls,0.16667,0.0>,<0.0,-0.08200,0.0>,0.0,PRIM_TEXTURE,3,k,<-nls,-0.16667,0.0>,<0.0,0.41801,0.0>,0.0,PRIM_TEXTURE,5,k,<nls,0.33333,0.0>,<0.0,-0.33201,0.0>,0.0]);    
            if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,0,k,<nls,0.33333,0.0>,<0.0,0.16800,0.0>,0.0,PRIM_TEXTURE,1,k,<nls,0.16667,0.0>,<0.0,-0.08200,0.0>,0.0,PRIM_TEXTURE,3,k,<-nls,-0.16667,0.0>,<0.0,0.41801,0.0>,0.0,PRIM_TEXTURE,5,k,<nls,0.33333,0.0>,<0.0,-0.33201,0.0>,0.0]);       
        }
    }
    if (t5!="") {
        k=(key)t5;
        if (k) {
            nas=(integer)as;
            if(nas<1 || nas>20) nas=1;
            s5=t5;
            if (LEFTARROW>0) llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_TEXTURE,0,k,<nas,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
            if (RIGHTARROW>0) llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_TEXTURE,0,k,<nas,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
        }
    }
    if (s2!="" || s3!="" || s5!="") {
        list li=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
        string desc=llList2String(li,0);
        //li=llParseString2List(desc,["#"],[]);
        li=llCSV2List(desc);
        integer n=llGetListLength(li);
        integer i;
        string t1="";
        string t2="";
        string t3="";
        for (i=0;i<n;i++) {
            if (llGetSubString(llList2String(li,i),0,1)=="t1") t1=llList2String(li,i);
            if (llGetSubString(llList2String(li,i),0,1)=="t2") t2=llList2String(li,i);
        }
        if (s2!="") t1="t1="+s2;        
        if (s5!="") t2="t2="+s5;        
        n=llSubStringIndex(desc,",t");
        string desc0;
        if(n>0) desc0=llGetSubString(desc,0,n-1);
        else if(n==0) desc0="";
        else desc0=desc;
        desc=desc0+","+t1+","+t2;
        llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,desc]);

        li=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_DESC]);
        desc=llList2String(li,0);
        n=llSubStringIndex(desc,"t3=");
        if(n>=0) t3=desc;
        if(s3!="") t3="t3="+s3; 
        if(t3!="") llSetLinkPrimitiveParams(LEFTLINE,[PRIM_DESC,t3]);

        if (s2!="" || s3!="") llMessageLinked(LINK_ALL_OTHERS,5000,"updtextchrono",s2+","+s3);
        if (s5!="") texarrow=(key)s5;
    }
}

getTextures() //establece texarrow
{
        list l=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
        string desc=llList2String(l,0);
        //l=llParseString2List(desc,["#"],[]);
        l=llCSV2List(desc);
        integer n=llGetListLength(l);
        integer i;
        key t1=NULL_KEY;    //texture numbers
        key t2=NULL_KEY;    //texture arrows
        key t3=NULL_KEY;    //texture modes
        for (i=0;i<n;i++) {
            if (llGetSubString(llList2String(l,i),0,1)=="t1") t1=(key)llGetSubString(llList2String(l,i),3,-1);
            if (llGetSubString(llList2String(l,i),0,1)=="t2") t2=(key)llGetSubString(llList2String(l,i),3,-1);
        }
        l=llGetLinkPrimitiveParams(CHRONO,[PRIM_TEXTURE,SIGN]);
        key k=llList2Key(l,0);
        if(k) t1=k;
        else if(t1) t1=t1;
        else llSay(0,"Display numbers texture is not available, load default textures");

        l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_TEXTURE,0]);
        k=llList2Key(l,0);
        if(k) t2=k;
        else if(t2) t2=t2;
        else llSay(0,"Arrow texture is not available, load default textures");

        n=llSubStringIndex(desc,",t");
        string desc0;
        if(n>0) desc0=llGetSubString(desc,0,n-1);
        else if(n==0) desc0="";
        else desc0=desc;
        desc0=desc0+",t1="+(string)t1+",t2="+(string)t2;
        if(desc0!=desc) llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,desc0]);

        l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_DESC]);
        desc=llList2String(l,0);
        n=llSubStringIndex(desc,"t3=");
        if(n>=0) t3=(key)llGetSubString(desc,n+3,-1);
        l=llGetLinkPrimitiveParams(CHRONO,[PRIM_TEXTURE,SUBTIT]);
        k=llList2Key(l,0);
        if(k) t3=k;
        else if(t3) t3=t3;
        else llSay(0,"Mode texture is not available, load default textures");
        desc0="t3="+(string)t3;
        if(desc0!=desc) llSetLinkPrimitiveParams(LEFTLINE,[PRIM_DESC,desc0]);

        llMessageLinked(LINK_ALL_OTHERS,5000,"updtextchrono",(string)t1+","+(string)t3);
        if(t2) texarrow=t2;
}

lineLength(float length) 
{
    if(length<8) length=8;
    else if(length>107) length=107;
    float n=length/2.0;
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector vl=llList2Vector(l,0);
    vl.x=n; 
    l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_SIZE]);
    vector va=llList2Vector(l,0);
    va.x=n; 
    n=n/2.0;   //position
    float pz=vl.z/2+va.z/2; //pos z arrow
    llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_SIZE,vl,PRIM_POS_LOCAL,<-n,0,0>]); 
    llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_SIZE,vl,PRIM_POS_LOCAL,<n,0,0>]); 
    llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_SIZE,va,PRIM_POS_LOCAL,<-n,0,pz>]); 
    llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_SIZE,va,PRIM_POS_LOCAL,<n,0,pz>]); 
    llMessageLinked(LINK_THIS, 0, "length", (string)length);  //for startline and fly
}

lineLengthAdjust(string cmd)
{
    float inc=0;
    if(cmd=="-") inc=-0.5;
    else if(cmd=="--") inc=-1.0;
    else if(cmd=="---") inc=-3.0;
    else if(cmd=="+") inc=0.5;
    else if(cmd=="++") inc=1.0;
    else if(cmd=="+++") inc=3.0;
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector vl=llList2Vector(l,0);
    float n=vl.x;  //actual length /2
    n+=inc/2.0;    //new length /2
    n=llFloor(llRound(n*100.0))/100.0;
    if (n<4.0) n=4.0;
    else if(n>54) n=54;
    vl.x=n; 
    l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_SIZE]);
    vector va=llList2Vector(l,0);
    va.x=n;
    float long=n*2.0;
    n=n/2.0; //position
    float pz=vl.z/2+va.z/2; //pos z arrow
    llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_SIZE,vl,PRIM_POS_LOCAL,<-n,0,0>]); 
    llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_SIZE,vl,PRIM_POS_LOCAL,<n,0,0>]); 
    llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_SIZE,va,PRIM_POS_LOCAL,<-n,0,pz>]); 
    llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_SIZE,va,PRIM_POS_LOCAL,<n,0,pz>]); 
    llMessageLinked(LINK_THIS, 0, "length", (string)long);    //for startline and fly
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
                    updateTextures(llList2String(l,1),llList2String(l,2),llList2String(l,3),llList2String(l,4),llList2String(l,5),llList2String(l,6),llList2String(l,7));
                    llRegionSay(pingChannel,"updtextext|"+llList2String(l,1)+"|"+llList2String(l,2)+"|"+llList2String(l,3)+"|"+llList2String(l,4)+"|"+llList2String(l,5)+"|"+llList2String(l,6)+"|"+llList2String(l,7));
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
        if(num>=0 && num<1000){
            if (str=="start") {
                if(lineStatus==0){ 
                    if(lineMode==3){
                        startN=num;
                        start((integer)((string)id));
                    }else start((integer)((string)id));
                }
                return;
            }        
            if (str=="restart") {
                restart((key)llGetSubString(id,0,35));    //id= id,time
                return;
            }
            if (str=="visible") {
                if (num==0) visible=0;
                else visible=1;
                return;
            }  
            if(lineMode==1){
                if (str=="modedata") {
                    list li=llCSV2List(id);
                    time1=(integer)llList2String(li,0);
                    time2=(integer)llList2String(li,1);
                    time3=(integer)llList2String(li,2);
                    return;
                }
                /*
                if (str=="dispways") {
                    dispWays(num);
                    return;
                } 
                */       
            }
            if (str=="setopt") {
                list parse=llCSV2List(id);
                setWays((integer)llList2String(parse,2));
                lineMode=(integer)llList2String(parse,5);
                raceTime=(integer)llList2String(parse,6);
                return;
            }
        }
        if(num<3000 && num>3999) return;  //Aux receive only between 3000 and 3999

        if (str=="setlength") {
            lineLength(num-3000);
            llMessageLinked(LINK_THIS, 7100, "linelength", (string)id+(string)(num-3000));
            return;
        }        
        if (str=="adjlength") {
            lineLengthAdjust((string)id);
            return;
        }   
        if (str=="prestart") {
            set_time=(integer)((string)id);
            prestart();
            return;
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
            if(lineMode==2 || lineMode==7) DisplayTime(start_at-now_timer);
            else DisplayTime(now_timer-start_at);  //negative during prestart
        }
        if (listenHandle>0 && listenTime<now_timer) {
            llListenRemove(listenHandle);
            listenHandle=0;
            listenTime=0;
            listenKey=NULL_KEY;
        }
        if(last_tick==0 && lineMode==5){
            if(lineStatus==1){
                llSetTimerEvent(0.0);            
                start(0);
            }else if(lineStatus==2){
                lineStatus=3;
                llSetTimerEvent(30.0);
            }else if(lineStatus==3){
                prestart();
            }
        }else if(last_tick==0 && lineMode==1 && swDispWays>0){ 
            if(swDispWays==1){
                llSetTimerEvent(30.0);
                swDispWays==2;
            }    
            if(now_timer-start_at>170){ 
                dispWays(0);
                swDispWays=0;
            }
        }else if(last_tick==0 && listenHandle==0){ 
            //dbgSay("close timer event Aux ");
            llSetTimerEvent(0.0); 
        }
    }
}