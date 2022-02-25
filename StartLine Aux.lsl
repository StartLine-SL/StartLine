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
//About this script - StartLine Aux ultimo
//StartLine version Sim
integer dbg=0;
integer model=0;    //0 sim model   1 fly model

integer channel=-8001;       //The channel to listen for start, reset and miscellanous other commands for nav huds
integer pingChannel=-2256845;   //the channel to listen for chronometers and others objects 
//integer flyChannel=-51234570;   
integer flyChannel=-51234580;   
integer helloChannel=-95456284;
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
integer startsay;
integer menuoutregion=0;
integer autounlock=1;
integer startN;
integer nethud=1;

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
integer bright=2;

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
key texnumbers;
key texmode;

integer lineMode;
integer time1;
integer time2;
integer time3;

//timer temporals vars
integer now_timer;

//default textures
key titletexture="08a3f265-5ac7-7e14-5b3f-d5e843e07d32";
key numberstexture="87a7adc0-8656-f64a-1628-f13f99f2070e";
key linetexture="d8b82064-17dc-8be0-ae6d-ef88f6f2d24b";
key arrowtexture="732b2f4d-ab20-64e9-5e25-c59c77b53dd4";
key modetexture="93067bbb-9922-32a5-742d-cc289fb6790a";
key buoytexturemain="";
key buoytexturering="";
key buoytexturead="";
float buoyadhscale=3.0;

integer linescale=3;
integer arrowscale=5;
float titlehscale=1;
float titlevscale=1;

vector flyposition;
integer flyreztime;
string flysimname;
float flychronoheight=9;
integer flylinerotation;
integer flydepth=-4;
integer flybuoys=1;
integer objnumber;
list objrez;

//END Global Variables/////////////////////////////////////////////

//Blocks/////////////////////////////////////////////
//Notecard Block->
//This block of functions reads a notecard

readfirstNCLine(string name) 
// Start reading a notecard line. Only call this when this notecard exists
{
    currentLine=0; 
    currentName=name;
    objnumber=0;
    objrez=[];
    if (llGetInventoryType(currentName)!=INVENTORY_NOTECARD){
        llSay(0, "Unable to find the '"+currentName+"' notecard. Default settings will be loaded.");
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

    if(dbg!=0) dbgSay("Settings Set: "+pText);

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
        if ((integer)value!=0) flyChannel=(integer)value; 
        return;
    }
    if (keystring=="admin") {
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
    if (keystring=="nethud") {
        nethud=(integer)value;
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
    if (keystring=="bright") {
        bright=(integer)value;
        return;
    }
    if (keystring=="netlink") { 
        return;
    }
    
    if (keystring=="titletexture"){ 
        if((key)value) titletexture=(key)value;
        return;
    }else if (keystring=="numberstexture"){ 
        if((key)value) numberstexture=(key)value;
        return;
    }else if (keystring=="linetexture"){ 
        if((key)value) linetexture=(key)value;
        return;
    }else if (keystring=="arrowtexture"){ 
        if((key)value) arrowtexture=(key)value;
        return;
    }else if (keystring=="modetexture"){ 
        if((key)value) modetexture=(key)value;
        return;
    }else if (keystring=="linescale"){
        if((integer)value>0) linescale=(integer)value;
        return;
    }else if (keystring=="arrowscale"){ 
        if((integer)value>0) arrowscale=(integer)value; 
        return;
    }else if (keystring=="titlehscale"){ 
        if((float)value>0) titlehscale=(float)value;
        return;
    }else if (keystring=="titlevscale"){ 
        if((float)value>0) titlevscale=(float)value;
        return;
    }
            
    if(model==1){
        if (keystring=="flyposition"){ flyposition=(vector)value; return;}
        else if (keystring=="flyreztime"){ flyreztime=(integer)value; return;}
        else if (keystring=="flysimname"){ flysimname=value; return;}
        else if (keystring=="flychronoheight"){ flychronoheight=(float)value; return;}
        else if (keystring=="flylinerotation"){ flylinerotation=(integer)value; return;}
        else if (keystring=="flydepth"){ flydepth=(integer)value; return;}
    
        if (llGetSubString(keystring,0,2)=="obj") {
            if (keystring=="objname"){
                objnumber++;
                objrez+=[value,"C",ZERO_VECTOR,ZERO_VECTOR,0.0,1,1,2];
                return; 
            }else if (keystring=="objpos"){ objrez=llListReplaceList(objrez,[value],(objnumber-1)*8+1,(objnumber-1)*8+1); return;} 
            else if (keystring=="objoffset"){ objrez=llListReplaceList(objrez,[(vector)value],(objnumber-1)*8+2,(objnumber-1)*8+2); return;} 
            else if (keystring=="objrot"){ objrez=llListReplaceList(objrez,[(vector)value],(objnumber-1)*8+3,(objnumber-1)*8+3); return;} 
            else if (keystring=="objsizefactor"){ objrez=llListReplaceList(objrez,[(float)value],(objnumber-1)*8+4,(objnumber-1)*8+4); return;} 
            else if (keystring=="objphantom"){ objrez=llListReplaceList(objrez,[(float)value],(objnumber-1)*8+5,(objnumber-1)*8+5); return;} 
            else if (keystring=="objalpha"){ objrez=llListReplaceList(objrez,[(float)value],(objnumber-1)*8+6,(objnumber-1)*8+6); return;} 
            else if (keystring=="objbright"){ objrez=llListReplaceList(objrez,[(float)value],(objnumber-1)*8+7,(objnumber-1)*8+7); return;} 
        }

        if (llGetSubString(keystring,0,3)=="buoy") {
            if (keystring=="buoytexturemain"){ 
                if((key)value) buoytexturemain=(key)value;
                return;
            }else if (keystring=="buoytexturering"){ 
                if((key)value) buoytexturering=(key)value;
                return;
            }else if (keystring=="buoytexturead"){ 
                if((key)value) buoytexturead=(key)value;
                return;
            }else if (keystring=="buoyadhscale"){ 
                if((float)value>0) buoyadhscale=(float)value;
                return;
            }
        }
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
            //llShout(channel,"starr"); //*****nofly
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
            llShout(channel,"starr,"+(string)start_at); //*****nofly
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
            if(model==0) llMessageLinked(LINK_ALL_OTHERS,5000+stage,"stage",""); 
            else if(model==1) llMessageLinked(LINK_THIS,9000+stage,"stage",""); 
            llMessageLinked(LINK_THIS,7200,"chanmsg","stage,"+(string)stage);
            llRegionSay(pingChannel,"stage,"+(string)stage);
            msg2="STAGE "+(string)stage+": "+msg2;
            if(msg!="") msg=msg2+"\n"+msg;
            else msg=msg2;
        }
    }        
    
    if(msg!=""){
        llMessageLinked(LINK_THIS,7200,"sendmsg2",msg);  
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
        if (start_at>=0){
            llMessageLinked(LINK_THIS,7202,"sendmsg2","");  //The race is over, timer is stopped.
            llMessageLinked(LINK_THIS,7200,"chanmsg","stopr");
        }else{
            llMessageLinked(LINK_THIS,7201,"sendmsg2","");  //Ready for race.
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
    if(lineMode==0){  //race
        lineStatus=0;
        start_at=p;
        //start command the third parameter is occupied by the hay.ax line sending the settings
        if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //shout to channel
        llMessageLinked(LINK_THIS,7204,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }else if(lineMode==1){  //match race
        lineStatus=0;
        start_at=p;
        if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //shout to channel
        llMessageLinked(LINK_THIS,7204,"sendmsg2","");
        swDispWays=1;
        stage=-1;
        dispWays(1);
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        //llMessageLinked(LINK_THIS,stage,"stage",""); 
        llSetTimerEvent(0.5);
    }else if(lineMode==2 || lineMode==7){  //trial series, trial  no start countdown
        lineStatus=0;
        start_at=p;
        if (lineMode==2) llMessageLinked(LINK_THIS,7207,"sendmsg2","");
        else llMessageLinked(LINK_THIS,7209,"sendmsg2","");
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
        if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+","+(string)startN);   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+","+(string)startN);   //shout to channel
        llMessageLinked(LINK_THIS,7208,"sendmsg2",(string)startN);  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }else if(lineMode==4){  //race training   no countdown 
        lineStatus=0;
        start_at=p;
        llMessageLinked(LINK_THIS,7206,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=0;
        //llSetTimerEvent(0.5);
    }else if(lineMode==5){   //start training
        llMessageLinked(LINK_THIS,7205,"sendmsg2","");  ///me : Countdown start, timer is running. shout channel 0 and relay channel
        lineStatus=2;
        last_tick=llGetUnixTime();
        start_at=last_tick+set_time;
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
        if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //shout to channel
        DisplayTime(last_tick-start_at);
        llSetTimerEvent(0.5);
    }else if(lineMode==6){  //network
        lineStatus=0;
        start_at=p;
        putColor(llGetUnixTime()-start_at);   
        if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //regionsay to channel
        if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)start_at+",,"+(string)lineMode+",0");   //shout to channel
        llMessageLinked(LINK_THIS,7204,"sendmsg2","");  ///The race start, timer is running. shout channel 0 and relay channel
        DisplayTime(llGetUnixTime()-start_at);
        last_tick=llGetUnixTime();
        llSetTimerEvent(0.5);
    }
}

prestart()
{
    if(lineMode==5){
        llMessageLinked(LINK_SET, 0, "restart", (string)NULL_KEY+","+(string)(-set_time));
        llRegionSay(pingChannel,"restartline,"+(string)(-set_time));
        lineStatus=1;
        llSetTimerEvent(30.0);
        llMessageLinked(LINK_THIS,7203,"sendmsg2","");   //30 seconds to start the countdown
    }
}

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
    //llRegionSay(pingChannel,"waysext|"+(string)ps); 
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
    if(model==0) putTextures();
    else if(model==1){
        s=(string)flyChannel+","+(string)flyposition+","+(string)flyreztime+","+flysimname+","+(string)flychronoheight+","+(string)flylinerotation+","+(string)flydepth;
        llMessageLinked(LINK_THIS, 9000, "flyconfig", s);
        
        if(objnumber>0){ 
            s=llList2CSV(objrez);
            llMessageLinked(LINK_THIS, 9000+objnumber, "flyobj",s);
        }
 
        s=(string)titletexture+","+(string)numberstexture+","+(string)modetexture+","+(string)linetexture+","+(string)arrowtexture+","+(string)linescale+","+(string)arrowscale+","+(string)titlehscale+","+(string)titlevscale;
        llMessageLinked(LINK_THIS, 9000, "flytexture", s);
        
        if(buoytexturemain!="" || buoytexturering!="" || buoytexturead!=""){
            s=(string)buoytexturemain+","+(string)buoytexturering+","+(string)buoytexturead+","+(string)buoyadhscale;
            llMessageLinked(LINK_THIS, 9000, "flytexbuoy", s);
        }
    }
    
    s=(string)channel+","+(string)pingChannel+","+(string)setDName+","+(string)autolock+","+(string)lockTimeDef+","+(string)autolockType+","+(string)alphaline+","+(string)colornormal+","+(string)dbg+","+(string)region_relay_channel+","+(string)autounlock+","+(string)menuoutregion+",1,"+(string)menutimeout+","+(string)model+","+(string)flyChannel+","+(string)bright+","+(string)helloChannel+","+(string)nethud;
    llMessageLinked(LINK_SET, 0, "config", s);  //send note options

    if(llGetListLength(owners)>0){
        ss=llList2CSV(owners);
        owners=[];
        if(ss!="") llMessageLinked(LINK_THIS, 4000, "admins", ss);  //send owners
    }

    if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,2,colornormal,0.0]); 
    if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_COLOR,ALL_SIDES,colornormal,alphaline,PRIM_COLOR,4,colornormal,0.0]); 

    if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    
    llSleep(2);
    s=(string)colorline+","+(string)colorchrono+","+(string)color61+","+(string)color60+","+(string)color30+","+(string)color10+","+(string)colorstart+","+(string)colornormal;
    llMessageLinked(LINK_SET, 5000, "colors", s);  //send colors to chrono
    llRegionSay(pingChannel,"colorsline,"+s);
    llRegionSay(pingChannel,"updtextchrono,"+(string)titletexture+","+(string)texnumbers+","+(string)texmode+","+(string)titlehscale+","+(string)titlevscale);
}

putTextures()
{
    if(titletexture){ 
        llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,0,titletexture,<titlehscale,titlevscale,0.0>,<0.0,0.0,0.0>,0.0]);    
    }
    if(numberstexture){
        texnumbers=numberstexture;
        llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,1,numberstexture,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,2,numberstexture,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,3,numberstexture,<1.0,1.0,0.0>,<0.74,0.0,0.0>,0.0,PRIM_TEXTURE,4,numberstexture,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0,PRIM_TEXTURE,5,numberstexture,<1.0,1.0,0.0>,<0.17,0.09766,0.0>,0.0,PRIM_TEXTURE,6,numberstexture,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
    }
    if(modetexture){ 
        texmode=modetexture;
        llSetLinkPrimitiveParamsFast(CHRONO,[PRIM_TEXTURE,7,modetexture,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);   
    } 
    if(linetexture){
        if(linescale<1 || linescale>20) linescale=3;
        if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_TEXTURE,0,linetexture,<linescale,0.33333,0.0>,<0.0,0.16800,0.0>,0.0,PRIM_TEXTURE,1,linetexture,<linescale,0.16667,0.0>,<0.0,-0.08200,0.0>,0.0,PRIM_TEXTURE,3,linetexture,<-linescale,-0.16667,0.0>,<0.0,0.41801,0.0>,0.0,PRIM_TEXTURE,5,linetexture,<linescale,0.33333,0.0>,<0.0,-0.33201,0.0>,0.0]);    
        if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_TEXTURE,0,linetexture,<linescale,0.33333,0.0>,<0.0,0.16800,0.0>,0.0,PRIM_TEXTURE,1,linetexture,<linescale,0.16667,0.0>,<0.0,-0.08200,0.0>,0.0,PRIM_TEXTURE,3,linetexture,<-linescale,-0.16667,0.0>,<0.0,0.41801,0.0>,0.0,PRIM_TEXTURE,5,linetexture,<linescale,0.33333,0.0>,<0.0,-0.33201,0.0>,0.0]);       
    }
    if (arrowtexture) {
        texarrow=arrowtexture;
        if(arrowscale<1 || arrowscale>20) arrowscale=5;
        if (LEFTARROW>0) llSetLinkPrimitiveParamsFast(LEFTARROW,[PRIM_TEXTURE,0,arrowtexture,<arrowscale,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
        if (RIGHTARROW>0) llSetLinkPrimitiveParamsFast(RIGHTARROW,[PRIM_TEXTURE,0,arrowtexture,<arrowscale,1.0,0.0>,<0.0,0.0,0.0>,0.0]);    
    }
    llMessageLinked(LINK_ALL_OTHERS,5000,"updtextchrono",(string)texnumbers+","+(string)texmode);
}

lineLength(float length)   //only model 0
{
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
}

setVisible()
{
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_COLOR,0]);
    vector c=llList2Vector(l,0);
    l=llGetLinkPrimitiveParams(LEFTARROW,[PRIM_COLOR,0]);
    vector a=llList2Vector(l,0);
    float f=0.0;
    if (ways!=0) f=1.0;
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

default 
{
    state_entry()
    {
        getLinkNums();
        //llMessageLinked(LINK_THIS, 1000, "ordmodel", "");
        if(model==0){
            titletexture="";
            linetexture="";
        }
        readfirstNCLine("StartLine settings");
    }

    link_message(integer sender_num, integer num, string str, key id)
    {
        if(num>=0 && num<1000){
            if (str=="start") {
                if(lineStatus==0){ 
                    if(lineMode==3){
                        startN=num;
                        start((integer)((string)id));
                    }else{ 
                        start((integer)((string)id));
                    }
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
                if(model==0) setVisible();
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
                return;
            }
        }
        if(num<3000 || num>3999) return;  //Aux receive only between 3000 and 3999

        if (str=="start") {
            if(lineMode==2 || lineMode==7 || lineMode==4){
                if(startsay==0 || startsay==2) llMessageLinked(LINK_THIS,7201,"chanmsg","start,"+(string)id+",,"+(string)lineMode+",0");   //regionsay to channel
                if(startsay==1 || startsay==2) llMessageLinked(LINK_THIS,7200,"chanmsg","start,"+(string)id+",,"+(string)lineMode+",0");   //shout to channel
            }
        }        

        if (str=="prestart") {
            set_time=(integer)((string)id);
            prestart();
            return;
        }        

        if(str=="length"){
            lineLength((float)((string)id));
            return;
        }

        if (str=="setlength") {
            float length=(float)llGetSubString(id,36,-1);
            if(length<8) length=8;
            else if(length>107) length=107;
            if(model==0) lineLength(length);
            else if(model==1) llMessageLinked(LINK_THIS, 9000, "length", (string)length);  //for fly
            llMessageLinked(LINK_THIS, 7100, "linelength", (string)id);
            return;
        }
        //if(str=="ansmodel"){
        //    model=(integer)((string)id);
        //    readfirstNCLine("StartLine settings");
        //}
        if(str=="colors"){
            list l=llCSV2List((string)id);
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
        /*
        if(str=="texarrow"){   //for model=2
            texarrow=id;
        }
        */
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
        }else if(last_tick==0){ 
            //if(dbg!=0) dbgSay("close timer event Aux ");
            llSetTimerEvent(0.0); 
        }
    }
}