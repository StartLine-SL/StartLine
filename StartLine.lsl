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
//'StartLine' is distributed in the hope that it will be useful
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
//Nums linkmessage:
//0-999 all scripts
//1000-1999 StartLine
//2000-2999 M
//3000-3999 Aux
//4000-4999 Menu
//5000-5999 Chrono and fly version
//6000-6999 send messages
//7000-7999 msg 
//9000-9999 fly version
//
//Free Codes: 1949,1952-1989,1991-1997
//StartLine version Sim
string linename="StartLine";
string version="2.0 ";
integer model;    //0 Sim  1 fly  set in the Aux script

integer dbg=0;      //is set in the Aux script
list calls=[0];  //list of calls  first element is 0   call num 21000
integer minTime=0;
integer pingChannel; //the channel to listen for chronometers and others objects, It is loaded from the StartLine Aux script
integer helloChannel;
integer helloHandle;
integer raceNumLaps=1;
integer ways=0;             //-1 -Y; +1 +Y; 0 Both
integer visible=1;
float alphaline=0.5;
integer set_time=120;       //The preset prestart time to start (sec)
integer setLockTime=1800;      //lock preset time for menu lock (sec) 
integer setLockTimeAdmin=1800;      //lock preset time for menu lock (sec) 
integer setDName;
integer autolock;
integer autounlock;
integer lockTimeDef;
integer autolockType;
vector colornormal = <0.539,0.6679,1.0>;
string nettype;   //espace   S start  F finish
key netlinekey;     //key de la propia startline
integer netlink=-2;  //link number   -2 undefined  -1 manual code  >=0 list number 
key netlinkKey;             //key de la linea de enlace
string netlinkRegion;
integer netconnect;
integer menuoutregion;
integer menutimeout;

integer nMenuTime;
integer nNetTime;
string  lockName="";
key  lockID=NULL_KEY;
integer locktime=-1; //time (seconds) the lockout ends  -1 no block 
integer lockType=0;  //Block type  0 no block   1 soft block   2 hard block
key unlockID=NULL_KEY;
integer lockMode=0;  //0 manual lock   1 auto lock   2 hud lock
integer lineMode;  //modo de la linea
string nameMode;  //nombre del modo
integer matchCD=300;   //match countdown  5'
integer matchOpen=240; //match ocs  4'
integer matchClose=60;  //match close ocs 
integer matchBad=30;   //match DSQ
integer raceCD=120;
integer raceTime=3600;
integer startNum=2;  //multistart number of start
integer startN; //multistart actual start 1,2,3,4
integer startF; //multistart 0-ok for prestart   1-prestart   2-start  3-finish all starts

integer LEFTLINE=0;
integer RIGHTLINE=0;
integer LEFTARROW=0;
integer RIGHTARROW=0;
integer CHRONO;
integer pingHandle;
integer start_at;           // The point in time (as unix time) when the start is planned / has been. -1 if not active
integer swRun=0;
float lineLen=80.0;  //fly
integer startRez;   //fky
integer linkType=LINK_ALL_OTHERS;  //LINK_ALL_OTHERS for model=0   LINK_THIS for model=1   send message to chrono or startline fly
string soptions;
integer bright=2;
integer nethud;
integer waiting;

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

//load the options saved in the chrono description
loadOptions(integer p) 
{
    list opt;
    if(model==1) opt=llGetLinkPrimitiveParams(2,[PRIM_DESC]); 
    else opt=llGetLinkPrimitiveParams(CHRONO,[PRIM_DESC]);
    opt=llCSV2List(llList2String(opt,0));
    if (llGetListLength(opt)>=10) {
        if((integer)llList2String(opt,0)>0) raceCD=(integer)llList2String(opt,0);
        if(llAbs((integer)llList2String(opt,1))<=1) ways=(integer)llList2String(opt,1);
        if((integer)llList2String(opt,2)>0) raceNumLaps=(integer)llList2String(opt,2);
        if((integer)llList2String(opt,3)>=0) minTime=(integer)llList2String(opt,3);
        if((integer)llList2String(opt,4)>=0) visible=(integer)llList2String(opt,4);
        if((integer)llList2String(opt,5)>0) setLockTime=(integer)llList2String(opt,5);
        if((integer)llList2String(opt,6)>0) setLockTimeAdmin=(integer)llList2String(opt,6);
        if((integer)llList2String(opt,7)>0) raceTime=(integer)llList2String(opt,7);
        if((integer)llList2String(opt,8)>0) matchCD=(integer)llList2String(opt,8);
        if((integer)llList2String(opt,9)>0) lineLen=(integer)llList2String(opt,9)/10.0;   //for model 1 only fly
        setMatch();
        if(p!=0){ //p=0 is not reset
            setVisible(visible);
            sendopt();
            if(lineMode==2 || lineMode==7) setTime(0,raceTime);
            else if(lineMode==1) setTime(0,matchCD);
            else setTime(0,raceCD);
        }
    }else if(p==0){
        //When starting the object, if the line does not have the options correctly, it saves them.
        saveOptions();
    }
}

//==> Menu 
//s1=0-raceNumLaps,1-ways,2-lineMode,3-set_time,4-visible,5-minTime,6-setLockTime,7-lockType,8-lockName,9-locktime,10-raceTime,11-startNum,
//        12-matchOpen,13-matchClose,14-matchBad,15-nameMode,16-nettype,17-netname,18-netconnect,19-netlinkRegion,20-setLockTimeAdmin
//s3=pId,lockID,isowner,startN,swRun,linename
DisplayMenu(key pId)
{
    string s1=(string)raceNumLaps+","+(string)ways+","+(string)lineMode+","+(string)set_time+","+(string)visible+","+(string)minTime+","+(string)setLockTime+","+(string)lockType+","+(string)lockName+","+(string)locktime+",";
    s1+=(string)raceTime+","+(string)startNum+","+(string)matchOpen+","+(string)matchClose+","+(string)matchBad+","+nameMode+","+nettype+",  ,";
    s1+=(string)netconnect+","+netlinkRegion+","+(string)setLockTimeAdmin;
    //string s2=(string)pId+","+(string)lockID+","+(string)isowner+","+(string)startN+","+(string)swRun+","+(string)startF+","+linename;
    string s2=(string)pId+","+(string)lockID+","+(string)startN+","+(string)swRun+","+(string)startF+","+linename;
    llMessageLinked(LINK_THIS,4992,s1,s2);   //to menu manage menu
} 

//s1=0-raceNumLaps,1-ways,2-lineMode,3-set_time,4-visible,5-minTime,6-setLockTime,7-lockType,8-lockName,9-locktime,10-raceTime,11-startNum,
//        12-matchOpen,matchClose,matchBad,15-nameMode,16-nettype,17-netname,18-netconnect,19-netlinkRegion,20-setLockTimeAdmin
//s2=type,pId,lockID,isowner,4-plugname,startRez,lineLen,7-nameMode;
SubMenu(string type, key pId)
{
    string s1=(string)raceNumLaps+","+(string)ways+","+(string)lineMode+","+(string)set_time+","+(string)visible+","+(string)minTime+","+(string)setLockTime+","+(string)lockType+","+(string)lockName+","+(string)locktime+",";
    s1+=(string)raceTime+","+(string)startNum+","+(string)matchOpen+","+(string)matchClose+","+(string)matchBad+","+nameMode+","+nettype+",  ,";
    s1+=(string)netconnect+","+netlinkRegion+","+(string)setLockTimeAdmin;
    //string s2=type+","+(string)pId+","+(string)lockID+","+(string)isowner+","+plugname+","+(string)startRez+","+(string)lineLen;
    string s2=type+","+(string)pId+","+(string)lockID+", ,"+(string)startRez+","+(string)lineLen+","+(string)startF;
    llMessageLinked(LINK_THIS,4991,s1,s2);   //to menu  manage submenu
}

setVisible(integer p)
{
    llRegionSay(pingChannel,"visline,"+(string)p);
    if(p==1){
        visible=1;
        llMessageLinked(LINK_SET, 10+ways, "visible", ""); 
    }else{
        visible=0;
        llMessageLinked(LINK_SET, 0, "visible", "");
    }
} 

integer timeInit()
{
    if(lineMode==2 || lineMode==7) return raceTime;  //2-Trial Series, 7-trial
    else if(lineMode==4) return 0;   //race training
    else return -set_time;  //0-race, 1-match, 3-multistart, 5-start training, 6-network
}   
    
Restart(integer pmode, key id)
{
    start_at=-1;
    if(model==0){
        llVolumeDetect(FALSE);
        llSetStatus(STATUS_PHANTOM, 1); 
    }
    integer vTimeInit=timeInit();
    //if(lineMode==1) setTime(0,vTimeInit);
    llMessageLinked(LINK_SET, 0, "restart", (string)id+","+(string)vTimeInit);
    llRegionSay(pingChannel,"restartline,"+(string)vTimeInit);
    nNetTime=0;
    if(autounlock && lockType){
        if (lockID!=id){ 
            //llInstantMessage(lockID, "The "+linename+" "+urlpos+" has been unlocked by "+sname+" secondlife:///app/agent/"+(string)id+"/im");
            llMessageLinked(LINK_ALL_OTHERS,7002,"unlockedby",(string)lockID+","+linename+","+(string)id); //instant message
        }
        lockID=NULL_KEY;
        lockType=0;
        lockName="";
        locktime=-1;
        llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
        llMessageLinked(LINK_THIS, 7100, "unlocked", id);
    }
}    

Start(key id) 
{
    llMessageLinked(LINK_ALL_OTHERS,6000,"name",llGetObjectName());
    start_at=-1;
    if(lineMode==0){   //race mode
        start_at=llGetUnixTime()+set_time;
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
    }else if(lineMode==1){   //match mode
        if(ways==0){
            llMessageLinked(LINK_THIS, 7100, "matchway", id);
            return;
        }
        llMessageLinked(LINK_SET,0,"modedata",(string)matchBad+","+(string)matchClose+","+(string)matchOpen);  
        llRegionSay(pingChannel,"modedata,"+(string)matchBad+","+(string)matchClose+","+(string)matchOpen);
        start_at=llGetUnixTime()+set_time;
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
    }else if(lineMode==2 || lineMode==7){   //Trial Series
        start_at=llGetUnixTime()+raceTime;  //end of time  *****
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
    }else if(lineMode==3){   //multistart
        startF=2;
        start_at=llGetUnixTime()+set_time;
        llMessageLinked(LINK_SET, startN, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,"+(string)startN+","+(string)start_at);
    }else if(lineMode==4){   //race train
        start_at=llGetUnixTime();
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
    }else if(lineMode==5){   //start train
        start_at=0;
        llMessageLinked(LINK_SET, 3000, "prestart", (string)set_time);  
    }else if(lineMode==6){   //network
        start_at=llGetUnixTime()+set_time;
        llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
        llRegionSay(pingChannel,"startline,0,"+(string)start_at);
        nNetTime=0;
    }
    if(start_at>=0){
        if(bright==2 && model!=1){
            if(lineMode!=3 || startN==1) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
        }
        if(lineMode==6 && netconnect==1) llMessageLinked(LINK_ALL_OTHERS,6000,"netremotestart",(string)start_at); 
        if(isCallsValue(2,7,0)) llMessageLinked(LINK_THIS,-21027,(string)start_at,(string)lineMode);    //call 
        llSetStatus(STATUS_PHANTOM, 0);
        llVolumeDetect(TRUE);
        if(dbg!=0) dbgSay("Detected START command with a point in time of "+(string)start_at);
        if(autolock) llMessageLinked(LINK_THIS,4000,"autolock",(string)id+","+(string)lockType+","+(string)locktime+","+(string)lockTimeDef+","+(string)autolockType);
    }
}

setMatch()
{
    if(matchCD==300){matchOpen=240;matchClose=60;matchBad=30;}
    else if(matchCD==240){matchOpen=180;matchClose=60;matchBad=30;}
    else if(matchCD==180){matchOpen=120;matchClose=60;matchBad=30;}
    else if(matchCD==120){matchOpen=120;matchClose=60;matchBad=30;}
    else if(matchCD==60){matchOpen=60;matchClose=30;matchBad=30;}
}

setTime(integer ptype, integer ptime)   //seconds 
{
    if(lineMode==2 || lineMode==7) raceTime=ptime;
    else{
        if(lineMode==1){  //match mode
            matchCD=ptime;
            setMatch();
        }else raceCD=ptime;
        set_time=ptime;
        if(lineMode==6){ 
            if(netconnect && ptype==0) llMessageLinked(LINK_ALL_OTHERS, 6000, "nettime",(string)set_time);  //network mode send set_time to remote line 
        }
    }
    string st=(string)timeInit();
    llMessageLinked(linkType,5000,"init",st);  
    llRegionSay(pingChannel,"initline,"+st);
}
//<== End Setting Options
//Send options to Aux and M
sendopt()
{
    string s=""+","+(string)minTime+","+(string)ways+","+(string)raceNumLaps+","+""+","+(string)lineMode+","+(string)timeInit()+","+(string)lineLen;
    llMessageLinked(LINK_THIS, 0, "setopt", s);  
}

//notice from menu: 0  from plugin execute: no execute 10 notice   execute 1 no notice 11 notice
fmode(integer pmode, string pname, key id, integer notice)  //change mode
{
    if(lineMode==6){  //if change mode reset network
        nettype=" ";
        netlink=-2;
        netlinkKey=NULL_KEY;
        netlinkRegion="";
        llMessageLinked(LINK_ALL_OTHERS,6000,"waiting",id);   //network mode stop waiting
        waiting=0;
        if(netconnect) llMessageLinked(LINK_ALL_OTHERS, 6000, "netdescon", "");
        netconnect=0;
        llMessageLinked(linkType,5000,"dispchron",nettype+","+(string)(-set_time));  //to chrono no waiting
        llRegionSay(pingChannel,"dispchron,0,"+nettype+","+(string)(-set_time));
        llMessageLinked(LINK_THIS, 4000, "netstopw", "");
    }
    if(notice==1 || notice==11 || (notice==0 && isCallsValue(1,pmode,2))){
        if(pmode!=0) raceNumLaps=1;
        if(pmode==1){
            set_time=matchCD;
        }else{ 
            set_time=raceCD;
            //if(pmode==2 || pmode==7) raceTime=3600;
            if(pmode==6){ 
                nettype=" ";
                netlink=-2;
                netlinkKey=NULL_KEY;
            }
        }
        lineMode=pmode;
        nameMode=pname;
        sendopt();
        llMessageLinked(LINK_ALL_OTHERS,lineMode,"mode","");   //to chrono and send results scripts
        llRegionSay(pingChannel,"mode,"+(string)lineMode);
        if(pmode==3){ 
            startN=1;
            startF=1;
        }
        Restart(0,NULL_KEY);
    }
    if(notice>=10 || (notice==0 && isCallsValue(1,pmode,1))){ 
        if(id) llMessageLinked(LINK_THIS, 7100, "mode", (string)pmode+","+pname+","+(string)id); 
    }
    if(notice==0 && isCallsValue(1,pmode,0)) llMessageLinked(LINK_THIS,-21010-pmode,pname,id); 
}

saveOptions()
{
    string s=(string)raceCD+","+(string)ways+","+(string)raceNumLaps+","+(string)minTime+","+(string)visible+","+(string)setLockTime+","+
            (string)setLockTimeAdmin+","+(string)raceTime+","+(string)matchCD+","+(string)((integer)(lineLen*10));
    if(model==1) llSetLinkPrimitiveParams(2,[PRIM_DESC,s]);
    else llSetLinkPrimitiveParams(CHRONO,[PRIM_DESC,s]);
}

inicio()
{
    if(pingChannel!=0) llRegionSay(pingChannel,"resetline");
    if(model==0){ 
        linkType=LINK_ALL_OTHERS;
        if(bright==1) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
        else llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]); 
        llShout(pingChannel,"bright,"+(string)bright);   
    }else{ 
        linkType=LINK_THIS;
    }
    llListenRemove(helloHandle);
    if(nethud==1 && helloChannel!=0) helloHandle=llListen(helloChannel, "", "", "");
    loadOptions(0);
    llSleep(1);
    Restart(0,NULL_KEY);
    //llSleep(1);
    setVisible(visible);
    sendopt();
    //setTime(0,set_time);
    setTime(0,raceCD);
    llSetTimerEvent(60.0);
    llMessageLinked(LINK_THIS, 7100, "msgversion", linename+","+version);
    if(pingChannel==0){ 
        llMessageLinked(LINK_THIS, 7100, "startbad", linename);   //"does not work because not started properly. Reset the line"
    }else{ 
        llMessageLinked(LINK_THIS, 7100, "ready", linename);   //linename+" is ready" 
        llMessageLinked(LINK_THIS, -20000, "ready", "");
        swRun=1;
    }
    if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
}


putCall(integer num, string code, string data)  //num=identifier
{
    //llOwnerSay((string)calls);
    integer hexpos=num%10;  //0-7
    if(hexpos>7) return;
    integer value=llList2Integer(calls,num/10) & (integer)("0x"+llInsertString("0000000",7-hexpos,"7"));
    if(value>15) value=value/(integer)llPow(16.0,hexpos);
    integer n;
    code=llGetSubString(code+"XX",0,2);
    if(llGetSubString(code,0,0)=="X") n=1;
    if(llGetSubString(code,1,1)=="X") n+=2;
    if(llGetSubString(code,2,2)=="X") n+=4;
    value=value & n;
    n=0;
    if(llGetSubString(code,0,0)=="1") n=1;
    if(llGetSubString(code,1,1)=="1") n+=2;
    if(llGetSubString(code,2,2)=="1") n+=4;
    value=value | n;
    n=llList2Integer(calls,num/10) & (integer)("0x"+llInsertString("FFFFFFF",7-hexpos,"0"));
    n=n | (integer)("0x"+llInsertString("0000000",7-hexpos,(string)value));
    calls=llListReplaceList(calls,[n],num/10,num/10);
    if(num==47) llMessageLinked(LINK_ALL_OTHERS,4000,"plugmenu","PL-"+data);    //plugMenu+=data;
    //llOwnerSay((string)calls);
}

//evaluate a bit of plugin calls list
integer isCallsValue(integer item, integer num, integer bit)  //item=item array, num=Hex pos 0-7, bit=bit 0,1,2  //return 0 or >0 
{
    return llList2Integer(calls,item) & (integer)("0x"+llInsertString("0000000",7-num,llGetSubString("124",bit,bit)));
}

putAction(integer num, string code, string data)
{
    if(num>=10 && num<=17){ 
        list l=["Race","Match","Trial Series","Multi Start","Race Training","Start Training","Network","Trial"];
        fmode(num-10,llList2String(l,num-10),(key)data,(integer)llGetSubString(code,1,2));  //values 01,10,11
    }else if(num==20) Start((key)data);
    else if(num==21) Restart(0,(key)data);
    else if(num==22) llMessageLinked(LINK_THIS, 2000, "results", "");
    else if(num==23){                    
        raceNumLaps=(integer)data;
        sendopt();
    }else if(num==24){
        ways=(integer)data;
        sendopt();
    }else if(num==25){ 
        setTime(0,(integer)data);
    }else if(num==26){
        minTime=(integer)data;
        sendopt();
    }else if(num==30){
        setVisible((integer)data);
    }else if(num==31){
        //llMessageLinked(LINK_ALL_OTHERS,5001,"loadopt",(string)model);
        loadOptions(1);
    }else if(num==40){
        llResetScript();
    }else if(num==41){
        llMessageLinked(LINK_THIS, 3000, "setlength", data);   //for model 0 and model 1
    }
    
}

default 
{
    state_entry()
    {
        integer n=llGetInventoryNumber(INVENTORY_SCRIPT);
        string s;
        while (n>0) {
            s=llGetInventoryName(INVENTORY_SCRIPT,n-1);
            if(s!=llGetScriptName() && s!="StartLineTemp"){ 
                llResetOtherScript(llGetInventoryName(INVENTORY_SCRIPT,n-1));
            }
            n--;
        }
        llMessageLinked(LINK_ALL_OTHERS, 0, "reset", NULL_KEY); 
        getLinkNums();
        for(n=1;n<5;n++) calls+=1717986918;  //0x66666666  list 5 integers 40 values
        llVolumeDetect(FALSE);
        llSetStatus(STATUS_PHANTOM, 1); 
        start_at=-1;
        llSetText("",ZERO_VECTOR,0);
        lineMode=0;
        nameMode="Race";
        //if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
        //llShout(0,"/me : Loading Data...");
    }

    touch_start(integer num_detected)
    {
        //only activated if the avatar is within the same sim as the line
        if(menuoutregion) DisplayMenu(llDetectedKey(0));
        else if(llGetAgentSize(llDetectedKey(0))) DisplayMenu(llDetectedKey(0));
    }
    
    changed(integer change)
    {
        if (change & CHANGED_REGION_START) {
            if(lineMode==6) {
                if (netconnect && nettype=="F" && start_at>0 && llGetUnixTime()<start_at+600) {
                    llMessageLinked(LINK_ALL_OTHERS, 6000, "netreconnect", "");    
                }
            }
        }
    }  
    
    listen(integer chan, string name, key id, string msg)    
    {
        //if(chan==helloChannel && nethud==1){
            if(waiting==1) llShout(helloChannel,"netError,1");  //1-The Line is waiting
            else if(start_at>=0) llShout(helloChannel,"netError,2");  //2-The Line is running
            else if(lockType>0 && lockID!=llGetOwnerKey(id)) llShout(helloChannel,"netError,4");  //4-The Line is locked
            else{
                if(msg=="netStartReq"){   //hud waiting start
                    if(waiting==11){
                        waiting=0;
                        llMessageLinked(LINK_ALL_OTHERS,6010,"waiting","");
                        llMessageLinked(linkType,5000,"dispchron",nettype+","+(string)(-set_time));  //to chrono
                        llRegionSay(pingChannel,"dispchron,0,  ,"+(string)(-set_time));
                        if(lineMode!=0){
                            fmode(0,"Race","",0);
                        }
                        llShout(helloChannel,"netStartAns,0");
                    }else if(waiting==0){
                        if(lineMode!=6){
                            fmode(6,"Network","",0);
                            llSleep(0.2);
                        }
                        waiting=11;
                        llMessageLinked(LINK_ALL_OTHERS,6011,"waiting","");
                        llMessageLinked(linkType,5001,"dispchron","T,"+(string)(-set_time));  //to chrono 5011 paint WS
                        llRegionSay(pingChannel,"dispchron,1,T,"+(string)(-set_time));
                    }else{
                        llShout(helloChannel,"netError,1");  //1-The Line is waiting
                    }
                }else if(msg=="netFinishReq"){   //hud waiting finish
                    if(waiting==12){
                        waiting=0;
                        llMessageLinked(LINK_ALL_OTHERS,6010,"waiting","");
                        llMessageLinked(linkType,5000,"dispchron",nettype+","+(string)(-set_time));  //to chrono
                        llRegionSay(pingChannel,"dispchron,0,  ,"+(string)(-set_time));
                        if(lineMode!=0){
                            fmode(0,"Race","",0);
                        }
                        llShout(helloChannel,"netFinishAns,0");
                    }else if(waiting==0){
                        if(lineMode!=6){
                            fmode(6,"Network","",0);
                            llSleep(0.2);
                        }
                        waiting=12;
                        llMessageLinked(LINK_ALL_OTHERS,6012,"waiting","");
                        llMessageLinked(linkType,5001,"dispchron","G,"+(string)(-set_time));  //to chrono 5011 paint WS
                        llRegionSay(pingChannel,"dispchron,1,G,"+(string)(-set_time));
                    }else{
                        llShout(helloChannel,"netError,1");  //1-The Line is waiting
                    }
                }
            }
        //}
    }      

    link_message(integer sender_num, integer num, string str, key id)
    {
        if(num>=21000 && num<22000){ 
            if(llGetSubString(str,0,0)=="2") putAction(num-21000,str,(string)id); //plugin calls execute action
            else putCall(num-21000,str,(string)id);  //plugin calls save code
            return;
        }
        if(num>=0 && num<1000){
            if (str=="config") {
                list l=llCSV2List(id); 
                pingChannel=(integer)llList2String(l,1);
                setDName=(integer)llList2String(l,2);
                autolock=(integer)llList2String(l,3);
                lockTimeDef=(integer)llList2String(l,4);
                autolockType=(integer)llList2String(l,5);
                alphaline=(float)llList2String(l,6);
                colornormal=(vector)llList2String(l,7);
                dbg=(integer)llList2String(l,8);
                //region_relay_channel=(integer)llList2String(l,9);
                autounlock=(integer)llList2String(l,10);
                menuoutregion=(integer)llList2String(l,11);
                menutimeout=(integer)llList2String(l,13);
                model=(integer)llList2String(l,14);
                bright=(integer)llList2String(l,16);
                helloChannel=(integer)llList2String(l,17);
                nethud=(integer)llList2String(l,18);
                inicio();
                return;
            }
        } 
        if(num<1000 || num>1999) return;
        if (num>1900) {
            if(num<1910){
                if(num==1901) DisplayMenu(id); 
                else if(num==1902) SubMenu(str,id); 
                else if(num==1903){  //change mode
                    fmode((integer)llGetSubString(str,0,0),llGetSubString(str,1,-1),id,0);
                    DisplayMenu(id);
                }
                return;
            }
            if(num==1950 || num==1951){  //unlock options
                if (str=="unlocksoft") {
                    if (lockID!=id) llMessageLinked(LINK_ALL_OTHERS,7002,"unlockedby",(string)lockID+","+linename+","+(string)id); //instant message
                    lockID=NULL_KEY;
                    lockType=0;
                    lockName="";
                    locktime=-1;
                    llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
                    llMessageLinked(LINK_THIS, 7100, "unlocked", id);
                    DisplayMenu(id);
                    return;
                }
                if (str=="unlockhard") {
                    if (lockID!=id) llMessageLinked(LINK_THIS,7002,"unlockedow",(string)lockID+","+linename+","+(string)id); //instant message
                    lockID=NULL_KEY;
                    lockType=0;
                    lockName="";
                    locktime=-1;
                    llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
                    llMessageLinked(LINK_THIS, 7100, "unlocked", id);
                    if(num==1951) DisplayMenu(id);
                    else SubMenu("admin",id);
                    return;
                }
                if (str=="unlockbad") {
                    llMessageLinked(LINK_THIS, 7100+num-1950, "lockednews", (string)id+","+lockName+","+(string)(locktime-llGetUnixTime())+","+(string)lockID);
                    return;
                }
            }            
            if(num==1911){   //visible
                if(isCallsValue(3,0,1)) llMessageLinked(LINK_THIS, 7100+(integer)str, "visible", id);
                if(isCallsValue(3,0,2)) setVisible((integer)str);
                if(isCallsValue(3,0,0)) llMessageLinked(LINK_THIS,-21030,str,id);    //call
                SubMenu("opt",id); 
                return;                
            }
            if(num==1912){   //results
                if(isCallsValue(2,2,2)) llMessageLinked(LINK_THIS, 2000, "results", "");   //execute
                if(isCallsValue(2,2,0)) llMessageLinked(LINK_THIS,-21022,"",id);    //call
                return;
            }  
            if(num==1913){   //start
                if(isCallsValue(2,0,2)) Start(id);   //execute
                if(isCallsValue(2,0,0)) llMessageLinked(LINK_THIS,-21020,"",id);    //call
                return;
            }     
            if(num==1914){   //Finish/Restart
                if(isCallsValue(2,1,2)){ 
                    startF=1;
                    startN=1;
                    if(lineMode==6) llMessageLinked(LINK_ALL_OTHERS,6000,"netrestart",id);   //network send restart to other line
                    Restart(0,id);   //execute
                }
                if(bright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
                if(isCallsValue(2,1,0)) llMessageLinked(LINK_THIS,-21021,"",id);    //call
                return;
            }
            if(num==1915){  //reset
                if(isCallsValue(4,0,1)) llMessageLinked(LINK_THIS, 7100, "resetline", id);
                if(isCallsValue(4,0,0)) llMessageLinked(LINK_THIS,-21040,"",id);    //call
                llSleep(1);
                if(isCallsValue(4,0,2)) llResetScript();   //execute
                return;
            }
            if(num==1916){  //save options
                saveOptions();
                llMessageLinked(LINK_THIS, 7100, "save", id);
                if(isCallsValue(4,2,0)) llMessageLinked(LINK_THIS,-21042,"",id);    //call
                SubMenu("admin",id);
                return;
            }
            if(num==1917){  //help
                if(isCallsValue(3,2,0)){ 
                    if (str=="0") llMessageLinked(LINK_THIS,-21032,"help",id);    //call
                    else if(str=="1") llMessageLinked(LINK_THIS,-21032,"license",id);    //call
                }
            }
            if(num==1918){  //direction
                integer w=0;
                if(str=="+Y") w=1;
                else if(str=="-Y") w=-1;
                if(isCallsValue(2,4,2)){
                    ways=w;
                    sendopt();
                }                
                if(isCallsValue(2,4,1)) llMessageLinked(LINK_THIS, 7100, "changedirection", (string)id+"|"+str);
                if(isCallsValue(2,4,0)) llMessageLinked(LINK_THIS,-21024,(string)w,id);    //call
                DisplayMenu(id);
                return;
            }
            if(num==1919){  //Lock
                if(lockType==0){
                    if (str=="1") locktime=llGetUnixTime()+setLockTimeAdmin;
                    else locktime=llGetUnixTime()+setLockTime;
                    lockID=id;
                    lockName=llKey2Name(id);
                    lockType=1;  //softlock 
                    lockMode=0;  //manual lock
                    llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
                    llMessageLinked(LINK_THIS, 7100, "lock",(string)id+","+lockName+","+(string)(locktime-llGetUnixTime()));                
                }else llMessageLinked(LINK_THIS, 7100, "islocked",id);
                SubMenu("opt",id);
                return;
            }
            if(num==1920){  //HardLock
                if(lockType==0){
                    locktime=llGetUnixTime()+setLockTimeAdmin;
                    lockID=id;
                    lockName=llKey2Name(id);
                    lockType=2;  //hardlock
                    lockMode=0;  //manual lock
                    llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
                    llMessageLinked(LINK_THIS, 7100, "hardlock",(string)id+","+lockName+","+(string)setLockTimeAdmin);
                } else llMessageLinked(LINK_THIS, 7100, "ishardlocked",id);
                SubMenu("admin",id);
                return;
            }
            if(num==1921){  //load default
                //if(isCallsValue(3,1,2)) llMessageLinked(LINK_ALL_OTHERS,5001,"loadopt",(string)model);   //execute
                if(isCallsValue(3,1,2)) loadOptions(1);   //execute
                if(isCallsValue(3,1,1)) llMessageLinked(LINK_THIS, 7100, "loaddefault", id);
                if(isCallsValue(3,1,0)) llMessageLinked(LINK_THIS,-21031,"",id);    //call
                DisplayMenu(id);
                return;
            }   
            if(num==1922){  //line length
                if(isCallsValue(4,1,2)) SubMenu("linelength",id);   //execute
                if(isCallsValue(4,1,0)) llMessageLinked(LINK_THIS,-21041,"",id);    //call
                return;
            }
            
            if(num==1925){    //racenumlaps
                if(isCallsValue(2,3,2)){ 
                    raceNumLaps=(integer)str;
                    sendopt();
                }
                if(isCallsValue(2,3,1)) llMessageLinked(LINK_THIS, 7100+raceNumLaps, "numlaps", id);
                if(isCallsValue(2,3,0)) llMessageLinked(LINK_THIS,-21023,str,id);    //call
                DisplayMenu(id);
                return;
            }
            if(num==1926){    //countdown
                if(isCallsValue(2,5,2)) setTime(0,(integer)str);
                if(isCallsValue(2,5,1)) llMessageLinked(LINK_THIS, 7100, "countdowntime", (string)id+str);
                if(isCallsValue(2,5,0)) llMessageLinked(LINK_THIS,-21025,str,id);    //call 
                DisplayMenu(id);
                return;
            }
            if(num==1927){    //minlaptime
                if(isCallsValue(2,6,2)){
                    minTime=(integer)str;
                    sendopt();
                }
                if(isCallsValue(2,6,1)) llMessageLinked(LINK_THIS, 7100, "minlaptime", (string)id+str);
                if(isCallsValue(2,6,0)) llMessageLinked(LINK_THIS,-21026,str,id);    //call 
                SubMenu("opt",id);
                return;
            }
            if(num==1928){    //locktime
                if(llGetSubString(str,0,0)=="1"){  //input for textbox for admin
                    setLockTimeAdmin=(integer)llGetSubString(str,1,-1);
                }else{ //input for menu option
                    setLockTime=(integer)llGetSubString(str,1,-1);
                    setLockTimeAdmin=setLockTime;
                }
                llMessageLinked(LINK_THIS, 7100, "locktime",(string)id+(string)setLockTimeAdmin); 
                SubMenu("opt",id);
                return;
            }

            if(lineMode==1){  //match race
                if(num==1930){    //open
                    matchOpen=(integer)str;
                    llMessageLinked(LINK_THIS, 7100, "matchopen", (string)id+str+" sec."); 
                    SubMenu("stages",id);
                    return;
                }
                if(num==1931){    //close
                    matchClose=(integer)str;
                    if(matchClose<10) matchClose=60*matchClose;
                    llMessageLinked(LINK_THIS, 7100, "matchclose", (string)id+(string)matchClose+" sec."); 
                    SubMenu("stages",id);
                    return;
                }
                if(num==1932){    //bad
                    matchBad=(integer)str;
                    if(matchBad<10) matchBad=60*matchBad;
                    llMessageLinked(LINK_THIS, 7100, "matchbad", (string)id+(string)matchBad+" sec."); 
                    SubMenu("stages",id);
                    return;
                }
            }

            if(lineMode==2){  //Trial Series
                if (num==1933) {   //starts number
                    raceTime=60*(integer)str;
                    setTime(0,raceTime);
                    //raceTime=150;   //for test
                    //llMessageLinked(LINK_ALL_OTHERS, 0, "restart", (string)id+","+(string)raceTime);     //Trial Series mode
                    llMessageLinked(LINK_THIS, 7100, "racetime", (string)id+(string)raceTime); 
                    DisplayMenu(id);
                    return;
                }
                
            }
            
            if(lineMode==3){  //multistart
                if (num==1934) {   //starts number
                    startNum=(integer)str;
                    llMessageLinked(LINK_THIS, 7100, "startnum", (string)id+(string)startNum); 
                    DisplayMenu(id);
                    return;
                }
                //startF 0-ok for prestart   1-prestart   2-start  3-finish all starts
                if (num==1935) {   //Start number
                    if((integer)str==startN){
                        if(model==1 && startRez==0) llMessageLinked(LINK_THIS, 7100, "flynobuild", id);    //First you have to press the 'Build Line' option    
                        else{
                            if (startF==3) llMessageLinked(LINK_THIS, 7100, "btnstartbad", id);   //Race is running, press Finish/Restart button
                            else if (startF==2) llMessageLinked(LINK_THIS, 7100, "btnmstartbad", id);   //Race is running, press Finish/Restart button
                            else{
                                if(isCallsValue(2,0,2)) Start(id);   //execute
                                if(isCallsValue(2,0,0)) llMessageLinked(LINK_THIS,-21020,"",id);    //call
                            }
                        }     
                    }
                    return;
                }
                if (num==1936) {   //preStart
                    if((integer)str==startN){  //verify start number
                        if(model==1 && startRez==0) llMessageLinked(LINK_THIS, 7100, "flynobuild", id);    //First you have to press the 'Build Line' option    
                        else{
                            if(isCallsValue(2,0,2)){
                                startF=1;
                                llMessageLinked(linkType,5000,"dispchron",(string)startN+","+(string)(-set_time));  //to chrono
                                llRegionSay(pingChannel,"dispchron,0,"+(string)startN+","+(string)(-set_time));
                            }
                            if(isCallsValue(2,0,0)) llMessageLinked(LINK_THIS,-21020,"",id);    //call
                            DisplayMenu(id);
                        }     
                    }else if((integer)str==startN+1){
                        if (startF==2) llMessageLinked(LINK_THIS, 7100, "btnmstartbad", id);
                    }
                    return;
                }
                if (num==1937) {   //Starts Timer submenu
                    if(startF==3){  //verify start number
                        SubMenu("starttime",id);
                    }else{
                        DisplayMenu(id);
                        llMessageLinked(LINK_THIS, 7100, "btnmstartbad", id);
                    }
                    return;
                }
                if (num==1947) {   //display timer
                    llMessageLinked(linkType,5000,"startchron",(string)str);  //to chrono
                    llRegionSay(pingChannel,"startchron,"+(string)str);
                }
            }            
            if (num==1938) {   //Results for multistart
                llMessageLinked(LINK_THIS,2000,"results",(string)startNum+str);   
                return;
            }

            if(lineMode==6){   //network
                if(num==1999){    //receive remote countdown
                    setTime(1,(integer)str);
                    llMessageLinked(LINK_THIS, 7100, "countdowntime", (string)id+str);
                    if(isCallsValue(2,5,0)) llMessageLinked(LINK_THIS,-21025,str,id);    //call 
                    return;
                }
                if(num==1998){    //receive remote disconnect
                    netconnect=0;
                    fmode(0,"Race","",0);
                    return;
                }
                if(num==1948){    //invert start/finish lines
                    if(nettype=="S" || nettype=="F") llMessageLinked(LINK_ALL_OTHERS, 6000, "netinvert", (string)nettype+(string)id); 
                    else llMessageLinked(LINK_THIS, 7100, "nettypeko", id);
                    return;
                }
            }
            
            if(lineMode==7){   //trail mode
                if (num==1945) {   //race time
                    raceTime=3600*(integer)str; //seconds 
                    //if(raceTime==21600) raceTime=150;   //for test 6h *****
                    setTime(0,raceTime);
                    //llMessageLinked(LINK_ALL_OTHERS, 0, "restart", (string)id+","+(string)raceTime);      
                    llMessageLinked(LINK_THIS, 7100, "racetime", (string)id+(string)raceTime); 
                    DisplayMenu(id);
                    return;
                }
            }
            
            if (num==1946){  //plugin option menu
                if(isCallsValue(4,7,0)) llMessageLinked(LINK_THIS,-21047,str,id);    //call    
            }

            if(num==1981){    //build and destroy line model=1
                startRez=(integer)str;
                if(startRez){
                    llMessageLinked(LINK_THIS,9000,"build",id);
                }else{
                    startF=1;
                    startN=1;
                    if(lineMode==6) llMessageLinked(LINK_ALL_OTHERS,6000,"netrestart",id);   //network send restart to other line
                    llMessageLinked(LINK_THIS,9000,"destroy",id);
                    fmode(0,"Race","",0);
                    Restart(1,id);
                }
                return;
            }
            
            if(num==1990){    //for test only
                llMessageLinked(LINK_ALL_OTHERS, 6000, "netreconnect", ""); 
            }
            return;
        }
        
        if(str=="length") {
            if(num==1000) lineLen=(float)llGetSubString(id,36,-1);
            else if(num==1001) lineLen+=(float)llGetSubString(id,36,-1); //adjust
            if(lineLen<8) lineLen=8;
            else if(lineLen>107) lineLen=107;
            //if(model==0) lineLength(lineLen);
            if(model==0) llMessageLinked(LINK_THIS, 3000, "length", (string)lineLen);
            else llMessageLinked(LINK_THIS, 9000, "length", (string)lineLen);  //for fly
            llMessageLinked(LINK_THIS, 7100, "linelength", llGetSubString(id,0,35)+(string)((integer)lineLen)+"."+llGetSubString((string)((integer)(lineLen*10)),-1,-1));
            return;
        }
        if (str=="raceend") {   //multistart mode finish start msg from startline engine
            if (startN<startNum) {
                startF=0;
                startN=num-1000+1;
            } else startF=3;
            return;
        }
        if(str=="linekey") {  //network actual line code
            netlinekey=id;
            return;
        }
        if (str=="netinvertex") {   //execute net invert type
            if(lineMode==6 && netconnect==1 && start_at<0){
                if(num==1000) llMessageLinked(LINK_ALL_OTHERS,6000,"netinvertres","OKnetinv"+nettype+llGetSubString(id,1,-1)); 
                nettype=llGetSubString(id,0,0);
                llMessageLinked(linkType,5000,"dispchron",nettype+","+(string)(-set_time));  //to chrono 
                llMessageLinked(LINK_THIS,2000,"nettype",(string)nettype);  //to engine
                llRegionSay(pingChannel,"dispchron,0,"+nettype+","+(string)(-set_time));
            }else{
                if(num==1000) llMessageLinked(LINK_ALL_OTHERS,6000,"netinvertres","KOnetinv"+(string)id);
            }
            return;
        } 
        if(str=="autolock") {  
            lockType=num-1000;
            if((string)id!=""){
                locktime=llGetUnixTime()+lockTimeDef;
                lockID=id;
                lockName=llKey2Name(id);
                lockMode=1; //autolock
                llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
            }
            return;
        }  
        if(str=="locktype") {  
            lockType=num-1000;
            return;
        }         
        if (lineMode==6) {
            if (str=="netrstart") {  //network receive remote start command
                if(netconnect){
                    start_at=(integer)((string)id);
                    llMessageLinked(LINK_SET, 0, "start", (string)start_at);  
                    llRegionSay(pingChannel,"startline,0,"+(string)start_at);
                    llSetStatus(STATUS_PHANTOM, 0);
                    llVolumeDetect(TRUE);
                    if(bright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
                }
                return;
            }
            if (str=="netstart") {   //network countdown is 0 line start
                if (nettype=="S") {
                    if (netconnect) {
                        llSetTimerEvent(60.0);
                        nNetTime=2;
                    }
                }
                return;
            }
            if (str=="netrtype") {  //from send results  mesage from finish line type Start or finish from netHud
                //0-type,1-remoteregion,2-remotekey,3-lockTime,4-avikey
                list l=llCSV2List(id);
                nettype=llList2String(l,0);
                llMessageLinked(linkType,5000,"dispchron",nettype+","+(string)(-set_time));  //to chrono
                llMessageLinked(LINK_THIS,2000,"nettype",(string)nettype);  //to engine
                llRegionSay(pingChannel,"dispchron,0,"+nettype+","+(string)(-set_time));
                if((integer)llList2String(l,3)>0){ //lock line
                    locktime=llGetUnixTime()+(integer)llList2String(l,3);
                    lockID=(key)llList2String(l,4);
                    lockName=llKey2Name(lockID);
                    lockType=1;  //softlock  es modo provisional 
                    lockMode=2;  //hud lock
                    llMessageLinked(LINK_THIS,4000,"hudlock",(string)lockID+","+(string)locktime);
                }
                netconnect=1;
                netlinkRegion=llList2String(l,1); 
                netlinkKey=(key)llList2String(l,2); 
                return;
            }
            if (str=="netrrestart") {   //network receive remote restart command
                if(isCallsValue(2,1,2)){ 
                    startF=1;
                    startN=1;
                    Restart(0,id);   //execute
                    if(bright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
                }
                if(isCallsValue(2,1,0)) llMessageLinked(LINK_THIS,-21021,"",id);    //call
                return;
            }
            if (str=="netsendstop") {   //network stop timer send data msg from engine
                nNetTime=0;
                return;
            }
            if (str=="netHudRestore") {   //network receive restore command from results
                list l=llCSV2List((string)id);
                if(start_at>=0) llMessageLinked(linkType,6001,"netRestoreError","");   //the line is running
                else if(lockType>0 && lockID!=(key)llList2String(l,1)) llMessageLinked(linkType,6002,"netRestoreError","");  //The Line is locked
                else if(netconnect==0) llMessageLinked(linkType,6003,"netRestoreError","");  //The Line is not connect
                else{
                    nettype=llList2String(l,0);
                    llMessageLinked(linkType,5001,"dispchron",nettype+","+(string)(-set_time));  //to chrono
                    llMessageLinked(LINK_THIS,2000,"nettype"," ");  //to engine
                    llRegionSay(pingChannel,"dispchron,1,"+nettype+","+(string)(-set_time));
                    netconnect=0;
                    netlinkRegion=""; 
                    netlinkKey=""; 
                    llMessageLinked(linkType,6000,"netHudRestore2",nettype+","+llList2String(l,2));
                    if(lockMode==2){  //hudlock
                        lockID=NULL_KEY;
                        lockType=0;
                        lockName="";
                        locktime=-1;
                        llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
                    }
                }
                return;
            }
        }
        if(str=="loptions2"){ soptions=(string)id; return; }
        //if(str=="ordmodel"){ llMessageLinked(LINK_THIS, 3000, "ansmodel", (string)model); return; }
    }
    

    timer() //each minute
    {
        integer now=llGetUnixTime();
        if(lockType>0 && now>locktime){  //unlock past time
            lockID=NULL_KEY;
            lockType=0;
            lockName="";
            locktime=-1;
            llMessageLinked(LINK_THIS,4000+lockType,"lockvars",lockID);
            llShout(0, "/me : "+"Line is now unlocked");
        }
        if(lineMode==6){  //network 
            if(nNetTime>0){ 
                if(nNetTime==1){  //Start sending data to remote line after 3 minutes of starting
                    llMessageLinked(LINK_THIS, 2000, "netsend", "");   //to engine script send 10 starts racers each minute
                }else{
                    --nNetTime;
                }
            }
        }
    }

    on_rez(integer whocares)
    {
        llResetScript();
    }
}