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
//About this script - StartLine
//It is the main script, when reset sends a reset to all other scripts
//Control all StartLine menus
//load default saved options
//It has the main values ​​of the options
//Set and Unset StartLine Status
//
integer dbg=0;
integer model=0;    //0 normal line     162 162 meters line 
string linename="StartLine";
string version="1.0";

//minTime is used to prevent people from crossing, turning around,
//and recrossing again without going around the course. it should be
//set to a large enough value in case someone accidentally recrosses,
//but not so large that someone who has raced around the course doesn't
//get counted.
integer minTime=30;
integer pingChannel; //the channel to listen for chronometers and others objects, It is loaded from the StartLine Aux script
integer raceNumLaps=1;
integer ways=0;             //-1 -Y; +1 +Y; 0 Both
integer visible=1;
float alphaline=0.5;
integer set_time=120;       //The preset prestart time to start (sec)
integer setLockTime=1800;      //lock preset time for menu lock (sec)
integer setDName;
integer autolock;
integer autounlock;
integer lockTimeDef;
integer autolockType;
vector colornormal = <0.539,0.6679,1.0>;
list owners;
integer menuoutregion;

integer nMenuTime;
string  lockName="";
key  lockID=NULL_KEY;
integer locktime=-1; //time (seconds) the lockout ends  -1 no block
integer lockType=0;  //Block type  0 no block   1 soft block   2 hard block
key unlockID=NULL_KEY;
integer lockMode=0;  //0 manual lock   1 auto lock

integer CENTERLINE=0;
integer LEFTLINE=0;
integer RIGHTLINE=0;
integer CHRONO=0;
integer SEND=0;
integer pingHandle;
integer listenHandle;
integer dialogChannel;
integer start_at;           // The point in time (as unix time) when the start is planned / has been. -1 if not active
vector COLOR_WHITE=<1.0,1.0,1.0>;
integer swRun=0;

sendMsg(key k, string s)
{
   llRegionSayTo(k,0,s);    
}

dbgSay(string text)
{
    if (dbg>0) llSay(0, text);
}

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
        else if (str==" ") 
            SEND=i;
    }
} 

//convert float time in seconds to "h:m:s" string
string sec2hms(float seconds) {
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

saveOptions()
{
    list l=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
    string s=llList2String(l,0);
    integer n=llSubStringIndex(s,"#t");
    if (n>=0) s=llGetSubString(s,n,-1);
    else s="";
    string v=(string)set_time+"#"+(string)ways+"#"+(string)raceNumLaps+"#"+(string)minTime+"#"+(string)visible+"#"+(string)setLockTime+s;    
    llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,v]);
}

//load the options saved in the chrono description
loadOptions(integer p) 
{
    list li=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
    string desc=llList2String(li,0);
    list opt=llParseString2List(desc, ["#"], []);
    integer len=llGetListLength(opt);
    if (llGetListLength(opt)>=6) {
        if((integer)llList2String(opt,0)>0) set_time=(integer)llList2String(opt,0);
        if(llAbs((integer)llList2String(opt,1))<=1) ways=(integer)llList2String(opt,1);
        if((integer)llList2String(opt,2)>0) raceNumLaps=(integer)llList2String(opt,2);
        if((integer)llList2String(opt,3)>0) minTime=(integer)llList2String(opt,3);
        if((integer)llList2String(opt,4)>=0) visible=(integer)llList2String(opt,4);
        if((integer)llList2String(opt,5)>0) setLockTime=(integer)llList2String(opt,5);
        if(p!=0){ //is not reset
            setVisible(visible);
            setTime(set_time);
        }
    }else if(p==0){
        saveOptions(); //When starting the object, if the line does not have the options correctly, it saves them.
    }
}

//build the current value of the options to display them in the menu
string viewSettings()
{
    string s="\n";
    s+="Num.Laps: "+(string)raceNumLaps+"\n";
    if (ways==0) s+="Way: Both\n";
    else if (ways==1) s+="Way: +Y\n";
    else if (ways==-1) s+="Way: -Y\n";
    s+="Countdown Time: "+sec2hms(set_time)+"\n";  
    if(visible==1) s+="Visible: ON\n";
    else s+="Visible: OFF\n";
    s+="Min. Lap Time: "+sec2hms(minTime)+"\n";
    s+="Lock Time: "+sec2hms(setLockTime)+"\n";
    if (lockType==0) s+="Locked: Unlocked";
    else if (lockType==1) s+="Locked by "+lockName+" for: "+sec2hms(locktime-llGetUnixTime())+"\n";
    else if (lockType==2) s+="Hard Locked by "+lockName+" for: "+sec2hms(locktime-llGetUnixTime())+"\n";
    return s;
}

//==> Menu
DisplayMenu(key pId)
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    integer index=-1;
    index=llListFindList(owners,[pId]);
    if (swRun==0){
        if(index>=0){
            s=linename+" does not work because not started properly. Reset the line";
            menu=["Close Menu","Reset"];
        }else{
            s=linename+" does not work because not started properly.";
            menu=["Close Menu"];
        }
    } else if (lockType==1 && lockID!=NULL_KEY && lockID!=pId && index==-1) {
        menu=["Close Menu","Unlock"];
        s=s+"This line is locked by "+lockName+" for "+sec2hms(locktime-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+(string)lockID+"/im before unlocking";
    } else if (lockType==2 && index==-1) {
        menu=["Close Menu"];
        s=s+"This line is locked by administrator "+lockName+" for "+sec2hms(locktime-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+(string)lockID+"/im if you need to override the lock";
    } else {
        if(visible==1){
            s="Invisible";
        }else{
            s="Visible";
        }
        menu=[" ","Help","===>","CountD. Time",s,"MinLapTime","Results","Num.Laps","Ways","Close Menu","Start","Finish/Restart"];
        s=viewSettings();
    }
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuOpt(key pId)
{
    list menu;
    string s="Lock";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    if (lockType==1) { 
        if (lockID==pId || llListFindList(owners,[pId])>=0) s="Unlock";
    }
    menu=["<===", s, "Owner Menu", "Close Menu", "Lock Time", "Load Default"];
    llDialog(pId, viewSettings(), menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuOwner(key pId)
{
    list menu;
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    string s="";
    if (model==0) s="Line Length";
    menu=["Hard Lock",s,"Load Texture","Save Opt.","Reset","Unlock.","Up."];
    llDialog(pId, viewSettings(), menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuHelp(key pId) 
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["Up","Help.","License"];
    s=s+"Chose one option...";
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuLineLength(key pId)     //Length
{
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector v=llList2Vector(l,0);
    float n=v.x*0.921875;
    l=llGetLinkPrimitiveParams(RIGHTLINE,[PRIM_SIZE]);
    v=llList2Vector(l,0);
    n+=v.x*0.921875;
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["90 m.","100 m.","118 m.","60 m.","70 m.","80 m.","30 m.","40 m.","50 m.","Up","Adjust"];
    integer int1=llFloor(n);
    integer int2=llRound(n*100.0)-(int1*100);
    s=s+"The current line length is "+(string)int1+"."+(string)int2+" m.";
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 


MenuAdjustLine(key pId)     //Adjust Line
{
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector v=llList2Vector(l,0);
    float n=v.x*0.921875;
    l=llGetLinkPrimitiveParams(RIGHTLINE,[PRIM_SIZE]);
    v=llList2Vector(l,0);
    n+=v.x*0.921875;
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["-","--","---","+","++","+++","Return"];
    integer int1=llFloor(n);
    integer int2=llRound(n*100.0)-(int1*100);
    s=s+"The current line length is "+(string)int1+"."+(string)int2+" m.";
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuLaps(key pId)
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["3 Laps","4 Laps","5 Laps","Up","1 Lap","2 Laps"];
    s=s+"Chose one option...";
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuTime(key pId)     //Countdown time
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["3 min.","4 min.","5 min.","Up","1 min.","2 min."];
    s=s+"The timer is set to "+sec2hms(set_time);
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 

MenuMinLapTime(key pId)
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["90 sec.","150 sec.","210 sec.","Up","30 sec.","60 sec."];
    s=s+"The minimum lap time is set to "+sec2hms(minTime);
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
}

MenuWays(key pId)
{
    string v;
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    if(ways==0) v="Both";
    else if(ways==1) v="+Y";
    else if(ways==-1) v="-Y";
    menu=["+Y","-Y","Both","Up"];
    s=s+"The Ways is set to "+v;
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
}

MenuLockTime(key pId)
{
    list menu;
    string s="\n";
    llListenRemove(listenHandle);
    listenHandle = llListen(dialogChannel, "", "", "");
    menu=["60 min","90 min","120 min","Up.","30 min","45 min"];
    s=s+"The lock time is set to "+sec2hms(setLockTime);
    llDialog(pId, s, menu,dialogChannel);
    nMenuTime=llGetUnixTime()+60;
} 
//<== End Menu
//==> setting options
setVisible(integer p)
{
    llRegionSay(pingChannel,"visline|"+(string)p);
    if(p==1){
        visible=1;
        llMessageLinked(LINK_SET, 10+ways, "visible", NULL_KEY);
    }else{
        visible=0;
        llMessageLinked(LINK_SET, 0, "visible", NULL_KEY);
    }
}

lineLength(integer length)
{
    float n=length/2;
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector v=llList2Vector(l,0);
    v.x=n/0.921875; 
    float pos=(v.x/2.0)-(v.x*0.078126);
    if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_SIZE,v,PRIM_POS_LOCAL,<-pos,0,0>]); 
    if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_SIZE,v,PRIM_POS_LOCAL,<pos,0,0>]); 
}

lineLengthAdjust(string cmd)
{
    list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
    vector v=llList2Vector(l,0);
    float n=v.x*0.921875;
    float inc=0;
    if(cmd=="-") inc=-0.5;
    else if(cmd=="--") inc=-1.0;
    else if(cmd=="---") inc=-3.0;
    else if(cmd=="+") inc=0.5;
    else if(cmd=="++") inc=1.0;
    else if(cmd=="+++") inc=3.0;
    n+=inc/2.0;
    n=llFloor(llRound(n*100.0))/100.0;
    if (n<4.0) n=4.0;
    else if(n>59) n=59;
    v.x=n/0.921875; 
    float pos=llFabs(v.x/2.0)-(v.x*0.078126); 
    if (LEFTLINE>0) llSetLinkPrimitiveParamsFast(LEFTLINE,[PRIM_SIZE,v,PRIM_POS_LOCAL,<-pos,0,0>]); 
    if (RIGHTLINE>0) llSetLinkPrimitiveParamsFast(RIGHTLINE,[PRIM_SIZE,v,PRIM_POS_LOCAL,<pos,0,0>]); 
}

Restart(key id)
{
    start_at=-1;
    llVolumeDetect(FALSE);
    llSetStatus(STATUS_PHANTOM, 1); 
    llMessageLinked(LINK_SET, -set_time, "restart", id);
    llRegionSay(pingChannel,"restartline|"+(string)(-set_time));
    if(autounlock && lockType){
        if (lockID!=id){ 
            string sname=llKey2Name(id);
            vector pos=llGetPos();
            string urlpos="http://maps.secondlife.com/secondlife/"+llEscapeURL(llGetRegionName())+"/"+(string)llRound(pos.x)+"/"+(string)llRound(pos.y)+"/"+(string)llRound(pos.z);
            llInstantMessage(lockID, "The "+linename+" "+urlpos+" has been unlocked by "+sname+" secondlife:///app/agent/"+(string)id+"/im");
        }
        lockID=NULL_KEY;
        lockType=0;
        lockName="";
        locktime=-1;
        sendMsg(id, "The line is now unlocked");
    }
}

Start(key id)
{
    integer now=llGetUnixTime();
    start_at=now+set_time;
    llMessageLinked(LINK_SET, start_at, "start", NULL_KEY);   
    llRegionSay(pingChannel,"startline|"+(string)start_at);
    llSetStatus(STATUS_PHANTOM, 0);
    llVolumeDetect(TRUE);
    dbgSay("Detected START command with a point in time of "+(string)start_at);
    if (autolock){
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
            locktime=llGetUnixTime()+lockTimeDef;
            lockID=id;
            lockName=llKey2Name(id);
            lockMode=1; //autolock
            sendMsg(id,"StartLine "+s+" by "+lockName+" for "+sec2hms(lockTimeDef));
        }
    }
}

setTime(integer ptime)   //seconds
{
    set_time=ptime;
    llMessageLinked(LINK_ALL_CHILDREN, -set_time, "init", NULL_KEY);  
    llRegionSay(pingChannel,"initline|"+(string)(-set_time));
}
//<== End Setting Options
//Send options to Aux
sendopt()
{
    string s="setopt|"+(string)setDName+"|"+(string)minTime+"|"+(string)ways+"|"+(string)raceNumLaps+"|"+(string)SEND;
    llMessageLinked(LINK_THIS, 0, s, NULL_KEY);  
}

inicio()
{
    llListenRemove(pingHandle);
    pingHandle = llListen(pingChannel, "", "", "");
    if(pingChannel!=0) llRegionSay(pingChannel,"resetline");
    Restart(NULL_KEY);
    llSleep(3);
    setVisible(visible);
    sendopt();
    setTime(set_time);
    llSetTimerEvent(60.0);
    llSay(0,linename+" version "+version);
    llSay(0,linename+" Copyright (C) see help file");
    llSay(0,linename+" comes with ABSOLUTELY NO WARRANTY");
    llSay(0,linename+" is free software (GPL-3.0 or later). See Licence file");
    if(pingChannel==0){ 
        llSay(0,linename+" does not work because not started properly. Reset the line");
    }else{ 
        llSay(0,linename+" is ready"); 
        swRun=1;
    }
    dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
}

default 
{
    state_entry()
    {
        integer n=llGetInventoryNumber(INVENTORY_SCRIPT);
        string s;
        while (n>0) {
            s=llGetInventoryName(INVENTORY_SCRIPT,n-1);
            if(s!=llGetScriptName()){ 
                llResetOtherScript(llGetInventoryName(INVENTORY_SCRIPT,n-1));
            }
            n--;
        }
        llMessageLinked(LINK_ALL_OTHERS, 0, "reset", NULL_KEY); 
        dialogChannel=-(integer)("0x"+llGetSubString((string)llGetKey(),-7,-1))-(integer)llFrand(1000000);
        getLinkNums();
        loadOptions(0);
        llVolumeDetect(FALSE);
        llSetStatus(STATUS_PHANTOM, 1); 
        start_at=-1;
        llSetText("",ZERO_VECTOR,0);
        owners=[llGetOwner()];
    }
    
    touch_start(integer num_detected)
    {
        //only activated if the avatar is within the same sim as the line
        if(menuoutregion){
            DisplayMenu(llDetectedKey(0));
        }else{
            if (llGetAgentSize(llDetectedKey(0))) DisplayMenu(llDetectedKey(0));
        }
    }

    listen(integer pchannel, string name, key id, string cmd) 
    {
        if (pchannel==dialogChannel) { //only menu channel is supported
            llListenRemove(listenHandle);
            string sname=llKey2Name(id);
            if(!menuoutregion){
                if (llGetAgentSize(id)) {} else return;   //only if the message comes from an avatar in the region
            }
            integer index=llListFindList(owners,[id]); 
    
            if (cmd == "Close Menu") {
                return;
            }
            
            //first unlock options
            if (lockType==1 && cmd=="Unlock") {
                if (lockID!=id){ 
                    vector pos=llGetPos();
                    string urlpos="http://maps.secondlife.com/secondlife/"+llEscapeURL(llGetRegionName())+"/"+(string)llRound(pos.x)+"/"+(string)llRound(pos.y)+"/"+(string)llRound(pos.z);
                    llInstantMessage(lockID, "The "+linename+" "+urlpos+" has been unlocked by "+sname+" secondlife:///app/agent/"+(string)id+"/im");
                }
                lockID=NULL_KEY;
                lockType=0;
                lockName="";
                locktime=-1;
                sendMsg(id, "The line is now unlocked");
                return;
            }
    
            if (lockType>0 && cmd=="Unlock.") {
                if (index>=0){
                    if (lockID!=id){ 
                        vector pos=llGetPos();
                        string urlpos="http://maps.secondlife.com/secondlife/"+llEscapeURL(llGetRegionName())+"/"+(string)llRound(pos.x)+"/"+(string)llRound(pos.y)+"/"+(string)llRound(pos.z);
                        llInstantMessage(lockID, "The "+linename+" "+urlpos+" has been unlocked by the owner "+sname+" secondlife:///app/agent/"+(string)id+"/im");
                    }
                    lockID=NULL_KEY;
                    lockType=0;
                    lockName="";
                    locktime=-1;
                    sendMsg(id, "The line is now unlocked");
                    MenuOwner(id);
                }
                return;
            }
            
            // Check for a lock, first
            if (lockType!=0 && lockID!=id && index<0 ) {
                sendMsg(id, "This line is locked by administrator "+lockName+" for "+sec2hms(locktime-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+(string)lockID+"/im if you need to override the lock");
                return;
            }
            
            if (cmd=="Up" || cmd=="<===") {
                DisplayMenu(id);
                return;
            }
    
            if (cmd=="Up.") {
                MenuOpt(id);
                return;
            }
    
            if (cmd == "Num.Laps") {
                MenuLaps(id);
                return;
            }
    
            if (llGetSubString(cmd,2,4)=="Lap") {   //NumLaps
                raceNumLaps=(integer)llGetSubString(cmd,0,0);
                sendopt();
                sendMsg(id, "New number of race laps: "+(string)raceNumLaps);
                DisplayMenu(id);
                return;
            }
    
            if (cmd=="CountD. Time") {  //menu countdown time
                MenuTime(id);
                return;
            }
    
            if (llGetSubString(cmd,2,5)=="min.") {   //Countdown Time
                setTime(60*(integer)llGetSubString(cmd,0,0));
                sendMsg(id,"Countdown time: "+sec2hms(set_time));
                DisplayMenu(id);
                return;
            }
    
            if (cmd == "MinLapTime") 
            { 
                MenuMinLapTime(id);
                return;
            }
            if (llSubStringIndex(cmd,"sec.")>0) {   //MinLapTime
                integer n=llSubStringIndex(cmd,"sec.");
                minTime=(integer)llGetSubString(cmd,0,n-1);
                sendopt();
                sendMsg(id, "New minimum lap time: "+sec2hms(minTime));
                DisplayMenu(id);
                return;
            }
    
            if (cmd == "===>") 
            {
                MenuOpt(id);
                return;
            }
    
            if (cmd == "Help") 
            {
                MenuHelp(id);
                return;
            }
            
            if(cmd=="Visible" || cmd=="Invisible"){
                if (cmd=="Visible") setVisible(1);
                else setVisible(0);
                DisplayMenu(id);
                return;
            }
    
            if (cmd=="Results") { 
                llMessageLinked(LINK_THIS, 0, "results", NULL_KEY);    
                return;
            }
            
            if (cmd=="Start") {
                if (start_at>0) sendMsg(id,"Race is running, press Finish/Restart button");
                else Start(id);
                return;
            }
    
            if (cmd == "Finish/Restart") {
                Restart(id);
                return;
            }
            
            if (cmd == "Reset") {
                sendMsg(id,"Reset line");
                llResetScript();
                return;
            }
            
            if(cmd=="Save Opt."){
                saveOptions();
                sendMsg(id,"Options have been saved");
                MenuOwner(id);
                return;
            }
            
            if (cmd == "Help.") {
                llGiveInventory(id,"StartLine readme"); 
                return;
            }
    
            if (cmd == "License") {
                llGiveInventory(id,"StartLine License");
                return;
            }
    
            if (cmd == "Ways") {
                MenuWays(id);
                return;
            }
    
            if (cmd == "+Y" || cmd=="-Y" || cmd=="Both") {
                if(cmd=="+Y") ways=1;
                else if(cmd=="-Y") ways=-1;
                else ways=0; 
                sendopt();
                sendMsg(id, "new way: "+cmd);
                DisplayMenu(id);
                return;
            }
    
            if (cmd == "Owner Menu") {
                if (llListFindList(owners, [id])>=0) {
                    MenuOwner(id);
                    return;
                } else {
                    sendMsg(id,"This option is for owners");
                }
            }
    
            if (cmd == "Lock Time") {
                MenuLockTime(id);
                return;
            }
            
            if (llGetSubString(cmd,-3,-1)=="min") {   //Lock Time
                setLockTime=60*(integer)llGetSubString(cmd,0,-4);
                sendMsg(id,"Lock time: "+sec2hms(setLockTime));
                MenuOpt(id);
                return;
            }
    
            if (cmd == "Lock") {
                if(lockType==0){
                    locktime=llGetUnixTime()+setLockTime;
                    lockID=id;
                    lockName=llKey2Name(id);
                    lockType=1;  //softlock 
                    lockMode=0;  //manual lock
                    sendMsg(id,"StartLine locked by "+lockName+" for "+sec2hms(locktime-llGetUnixTime()));
                } else sendMsg(id,"This line is locked, unlock it first before locking it again");     
                MenuOpt(id);
                return;
            }
    
            if (cmd == "Hard Lock") {
                if(lockType==0){
                    locktime=llGetUnixTime()+setLockTime;
                    lockID=id;
                    lockName=llKey2Name(id);
                    lockType=2;  //hardlock
                    lockMode=0;  //manual lock
                    sendMsg(id,"StartLine Hard locked by "+lockName+" for "+sec2hms(locktime-llGetUnixTime()));
                } else sendMsg(id,"This line is locked, unlock it first before locking it again");     
                MenuOwner(id);
                return;
            }
    
            if (cmd == "Load Default") {
                loadOptions(1);
                sendMsg(id,"Default options have been loaded");
                MenuOpt(id);
                return;
            }
    
            if (cmd == "Line Length") {
                if (model==0) MenuLineLength(id);
                return;
            }
    
            if (llGetSubString(cmd,-2,-1) == "m.") {
                lineLength((integer)llGetSubString(cmd,0,2));
                MenuOwner(id);
                return;
            }

            if (cmd == "Return") {
                MenuLineLength(id);
                return;
            }

            if (cmd == "Adjust") {
                MenuAdjustLine(id);
                return;
            }
            
            if (cmd=="-" || cmd=="--" || cmd=="---" || cmd=="+" || cmd=="++" || cmd=="+++") {
                lineLengthAdjust(cmd);
                MenuAdjustLine(id);
                return;
            } 

            if (cmd == "Load Texture") {
                llMessageLinked(LINK_THIS, 0, cmd, id);                  
                return;
            }
        } else if (pchannel==pingChannel) {

            if (llGetSubString(cmd,0,7)=="crossing") {
                llMessageLinked(LINK_THIS, 0, cmd, NULL_KEY);                  
                return;
            }

            if (cmd == "extreq") {
                string s="configext|"+(string)ways+"|"+(string)visible+"|"+(string)alphaline+"|"+(string)colornormal+"|"+(string)setDName+"|"+(string)dbg;
                llRegionSay(pingChannel,s);
                return;
            }

            if (llGetSubString(cmd,0,7) == "exttouch") {
                key k=(key)llGetSubString(cmd,9,-1);
                if (llKey2Name(k)!="") DisplayMenu(k);
                return;
            }
        }
    }

    link_message(integer sender_num, integer num, string str, key id)
    {
        if (llGetSubString(str,0,5)=="config") {
            list l=llParseString2List(str,["|"],[]); 
            pingChannel=(integer)llList2String(l,2);
            setDName=(integer)llList2String(l,3);
            autolock=(integer)llList2String(l,4);
            lockTimeDef=(integer)llList2String(l,5);
            autolockType=(integer)llList2String(l,6);
            alphaline=(float)llList2String(l,7);
            colornormal=(vector)llList2String(l,8);
            dbg=(integer)llList2String(l,9);
            //region_relay_channel=(integer)llList2String(l,10);
            autounlock=(integer)llList2String(l,11);
            menuoutregion=(integer)llList2String(l,12);
            if (id!=""){
                l=llParseString2List(id,["#"],[]);
                integer n=llGetListLength(l);
                key ow;
                while (n>0) { 
                    ow=(key)llList2String(l,n-1);
                    if(ow) owners+=ow;
                    n--;
                }
            }
/*
            if (llList2String(l,14)!=""){
                l=llParseString2List(llList2String(l,14),["#"],[]);
                integer n=llGetListLength(l);
                while (n>0) { 
                    owners+=(key)llList2String(l,n-1);
                    n--;
                }
            }
*/            
            inicio();
            return;
        }        
    }
    


    timer()
    {
        integer now=llGetUnixTime();
        if (lockType>0 && now>locktime) {  //unlock past time
            lockID=NULL_KEY;
            lockType=0;
            lockName="";
            locktime=-1;
            llShout(0, "/me : "+"Line is now unlocked");
        }
        if (nMenuTime>0 && now>nMenuTime) { //disable menu
            nMenuTime=0;
            llListenRemove(listenHandle); 
        }
    }

    on_rez(integer whocares)
    {
        llResetScript();
    }
}