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
//About this script - StartLine Menu
//MessateLinked numbers: 4000-4999

integer dbg;
integer menutimeout;
integer menuoutregion;
integer lineMode;
integer resultsMode;
integer start_at=-1;
integer startRez;
list plugMenu;  //menu de pluguins, lista de los plugins
integer nPlugMenu;  //numero de plugins de la lista
integer lockType;
key lockID;
list owners;

integer dialogChannel;
integer dialogHandle;
integer textboxChannel;
integer textboxHandle;

integer LEFTLINE;
integer RIGHTLINE;

integer swWaiting;
integer model;



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

string sec2hms(integer seconds) 
{
    string hms;
    if (seconds<0) hms="-";
    else if (seconds==0) return "0s ";
    else hms="";
    seconds=llAbs(seconds);
    integer hr=(integer)(seconds/3600.0);
    integer mn=(integer)((seconds-hr*3600)/60.0);
    integer sc=(integer)(seconds-(mn*60)-(hr*3600));
    if(hr>0) hms+=(string)hr+"h ";
    if(mn>0 || (hr>0 && sc>0)) hms+=(string)mn+"m ";
    if(sc>0) hms+=(string)sc+"s ";
    return hms;
} 

//ps1=0-raceNumLaps,1-ways,2-lineMode,3-set_time,4-visible,5-minTime,6-setLockTime,7-lockType,8-lockName,9-locktime,10-raceTime,11-startNum,
//        12-matchOpen,matchClose,matchBad,15-nameMode,16-nettype,17-netname,18-netconnect,19-netlinkRegion,20-setLockTimeAdmin
//ps3=0-pId,1-lockID,2-startN,3-swRun,4-startF,5-linename
DisplayMenu(string ps1, string ps3)   //main menu  page one
{
    list ls1=llCSV2List(ps1);
    list ls3=llCSV2List(ps3);
    list menu;
    string s="\n";
    integer isowner=0;
    if(llListFindList(owners,[(key)llList2String(ls3,0)])>=0) isowner=1;    
    
    if(llList2String(ls3,3)=="0"){   //if swRun 
        s=llList2String(ls3,5)+" does not work because not started properly.";
        if(isowner==1){
            s+=" Reset the line";
            menu=["Close Menu","Reset"];
        }else{
            menu=["Close Menu"];
        }
    }else if((integer)llList2String(ls1,7)>0){
        if(llList2Key(ls3,1)!=llList2Key(ls3,0)){   //idlocked != id
            if(llList2String(ls1,7)=="1") {   //soft lock
                menu=["Close Menu","(Unlock)"];
                s=s+"This line is locked by "+llList2String(ls1,8)+" for "+sec2hms((integer)llList2String(ls1,9)-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+llList2String(ls3,1)+"/im before unlocking";
            }else if(llList2String(ls1,7)=="2" && isowner==0){   //hard lock and is not admin
                menu=["Close Menu"];
                s=s+"This line is locked by administrator "+llList2String(ls1,8)+" for "+sec2hms((integer)llList2String(ls1,9)-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+llList2String(ls3,1)+"/im if you need to override the lock";
            }else if(llList2String(ls1,7)=="2" && isowner==1){   //hard lock and is admin
                menu=["Close Menu","(Unlock.)"];
                s=s+"This line is locked by administrator "+llList2String(ls1,8)+" for "+sec2hms((integer)llList2String(ls1,9)-llGetUnixTime())+". Please contact to him/her secondlife:///app/agent/"+llList2String(ls3,1)+"/im before unlocking";
            }
        }
    }
    if(llGetListLength(menu)==0){
        string s0="Start";
        string s1="CountDown...";
        string s2="  "; //two spaces
        string s3="Results";
        string s4="  ";
        if(llList2String(ls1,2)=="0") s4="Num.Laps...";
        else if(llList2String(ls1,2)=="1") s2="Stages...";    //mode match
        else if(llList2String(ls1,2)=="2" || llList2String(ls1,2)=="7") s1="Race Time...";   //mode trial series  and trial
        else if(llList2String(ls1,2)=="3"){   //mode multistart
            s2="Num.Starts...";
            s3+="...";
            if(llList2String(ls3,4)=="0") s0="PreStart "+llList2String(ls3,2);
            else if(llList2String(ls3,4)=="1") s0="Start "+llList2String(ls3,2);
            else if(llList2String(ls3,4)=="2"){
                if((integer)llList2String(ls3,2)<(integer)llList2String(ls1,11)) s0="PreStart "+(string)((integer)llList2String(ls3,2)+1);
                else s0="Starts Time...";
            }else if(llList2String(ls3,4)=="3") s0="Starts Time...";
        }else if(llList2String(ls1,2)=="6") {
            if(llList2String(ls1,18)=="0") s0="  ";
            s2="Network...";  //mode network
        }
        menu=["Mode...", s2, "===>", s1, "  ", "Load Default", s3, s4, "Direction...", "Close Menu", s0, "Finish/Restart"];
        //menu=["Mode...", s2, "===>", s1, "Test", "Load Default", s3, s4, "Direction...", "Close Menu", s0, "Finish/Restart"];
        s="\nStartLine Main menu"+viewSettings(ps1,isowner);
    }
    //llMessageLinked(LINK_THIS,1991,llDumpList2String(menu,","),llList2String(ls3,0)+s);   

    llListenRemove(dialogHandle); 
    dialogHandle = llListen(dialogChannel, "", "", "");
    llDialog((key)llList2String(ls3,0),s,menu,dialogChannel);
    if(menutimeout>0) llSetTimerEvent(menutimeout);
} 

//ps1=0-raceNumLaps,1-ways,2-lineMode,3-set_time,4-visible,5-minTime,6-setLockTime,7-lockType,8-lockName,9-locktime,10-raceTime,11-startNum,
//        12-matchOpen,13-matchClose,14-matchBad,15-nameMode,16-nettype,17-netname,18-netconnect,19-netlinkRegion,20-setLockTimeAdmin
//build the current value of the options to display them in the menu
string viewSettings(string ps1, integer pisowner)
{
    list ls1=llCSV2List(ps1);
    string s="\nMode: "+llList2String(ls1,15);
    s+="\nNum.Laps: "+llList2String(ls1,0);
    if (llList2String(ls1,1)=="1") s+="\nDirection: +Y";
    else if (llList2String(ls1,1)=="-1") s+="\nDirection: -Y";
    else s+="\nDirection: Both";
    if(llList2String(ls1,2)=="2") s+="\nTrial Series mode Race Time: "+sec2hms((integer)llList2String(ls1,10)); 
    else if(llList2String(ls1,2)=="7") s+="\nTrial mode Race Time: "+sec2hms((integer)llList2String(ls1,10)); 
    else s+="\nCountdown Time: "+sec2hms((integer)llList2String(ls1,3)); 
    if(llList2String(ls1,2)=="1"){
        s+="\nStage 1 Open time: "+sec2hms((integer)llList2String(ls1,12)); 
        s+="\nStage 2 Close time: "+sec2hms((integer)llList2String(ls1,13)); 
        s+="\nStage 3 Bad time: "+sec2hms((integer)llList2String(ls1,14)); 
    }else if(llList2String(ls1,2)=="3"){
        s+="\nMultiStart mode Number of Starts: "+llList2String(ls1,11); 
    }else if(llList2String(ls1,2)=="6"){
        s+="\nConnected with: ";
        if(llList2String(ls1,18)=="1") s+=llList2String(ls1,19)+" sim";
        else s+="Not Connected";
        //s+="\nNet Link name: "+llList2String(ls1,17);
        s+="\nNet Line type: ";
        if(llList2String(ls1,16)=="S") s+="Start";
        else if(llList2String(ls1,16)=="F") s+="Finish";
    }
    if(llList2String(ls1,4)=="1") s+="\nVisible: ON";
    else s+="\nVisible: OFF";
    s+="\nMin. Lap Time: "+sec2hms((integer)llList2String(ls1,5));
    s+="\nLock Time: ";
    if(pisowner) s+=sec2hms((integer)llList2String(ls1,20));
    else s+=sec2hms((integer)llList2String(ls1,6));
    if (llList2String(ls1,7)=="0") s+="\nLocked: Unlocked";
    else if (llList2String(ls1,7)=="1") s+="\nLocked by "+llList2String(ls1,8)+" for: "+sec2hms((integer)llList2String(ls1,9)-llGetUnixTime());
    else if (llList2String(ls1,7)=="2") s+="\nHard Locked by "+llList2String(ls1,8)+" for: "+sec2hms((integer)llList2String(ls1,9)-llGetUnixTime());
    return s;
}
 
//ps1=0-raceNumLaps,1-ways,2-lineMode,3-set_time,4-visible,5-minTime,6-setLockTime,7-lockType,8-lockName,9-locktime,10-raceTime,11-startNum,
//        12-matchOpen,matchClose,matchBad,15-nameMode,16-nettype,17-netname,18-netconnect,19-netlinkRegion,20-setLockTimeAdmin
//ps2=0-type,1-pId,2-lockID,3-plugname,4-startRez,5-lineLen,6-startF;
SubMenu(string ps1, string ps2)
{
    list ls1=llCSV2List(ps1);
    list ls2=llCSV2List(ps2);
    string type=llList2String(ls2,0);
    list menu;
    string s="\n";
    integer isowner=0;
    if(llListFindList(owners,[(key)llList2String(ls2,1)])>=0) isowner=1;    

    if(type=="opt"){  //main menu pag two
        string vs1;
        if(llList2String(ls1,4)=="1") vs1="Invisible";
        else vs1="Visible";
        string vs2="Lock";
        string vs3="  ";
        string vs4="  ";
        if(llListFindList(owners,[(key)llList2String(ls2,1)])>=0) vs3="Admin Menu...";    

        if (llList2String(ls1,7)=="1"){
            if ((key)llList2String(ls2,2)==(key)llList2String(ls2,1) || isowner==1) vs2="Unlock";
        } 
        if(nPlugMenu>0){
            if(nPlugMenu==1) vs4=llList2String(plugMenu,0);
            else vs4="Plugins";
        } 
        //menu=["<===", vs2, "Admin Menu", "Lock Time", "MinLapTime", vs1, "Close Menu"];
        menu=["<===", "Help...", vs3, "MinLapTime...", vs1, vs4, "Close Menu", vs2, "Lock Time..."];
        s+="StartLine Secondary menu"+viewSettings(ps1,isowner);
    }else if(type=="admin"){
        if(llListFindList(owners,[(key)llList2String(ls2,1)])>=0){    
            menu=["Hard Lock","Line Length"," ","Save Default","Reset","Unlock.","Up."];
            if (model==1){ 
                if(llList2String(ls2,4)=="0") s="Build Line";
                else s="Destroy Line";
                menu=[s]+menu; 
            }
            s+="StartLine Admin menu"+viewSettings(ps1,isowner);
        }
    }else if(type=="help"){
        menu=["Up","Help.","License"];
        s=s+"StartLine Help menu";
    }else if(type=="linelength"){
        integer int1;
        integer int2;
        if(model==1){
            int1=(integer)llList2String(ls2,5);
            int2=llRound((float)llList2String(ls2,5)*100.0)-(int1*100);
        }else{
            list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
            vector v=llList2Vector(l,0);
            float n=v.x;
            l=llGetLinkPrimitiveParams(RIGHTLINE,[PRIM_SIZE]);
            v=llList2Vector(l,0);
            n+=v.x;
            int1=llFloor(n);
            int2=llRound(n*100.0)-(int1*100);
        }
        menu=["90 m.","100 m.","107 m.","60 m.","70 m.","80 m.","30 m.","40 m.","50 m.","Admin Menu...","Adjust"];
        s+="StartLine Line Length menu\nThe current line length is "+(string)int1+"."+(string)int2+" m.";
    }else if(type=="adjustline"){
        integer int1;
        integer int2;
        if(model==1){
            int1=(integer)llList2String(ls2,5);
            int2=llRound((float)llList2String(ls2,5)*100.0)-(int1*100);
        }else{
            list l=llGetLinkPrimitiveParams(LEFTLINE,[PRIM_SIZE]);
            vector v=llList2Vector(l,0);
            float n=v.x;
            l=llGetLinkPrimitiveParams(RIGHTLINE,[PRIM_SIZE]);
            v=llList2Vector(l,0);
            n+=v.x;
            int1=llFloor(n);
            int2=llRound(n*100.0)-(int1*100);
        }
        menu=["-","--","---","+","++","+++","Return"];
        s+="StartLine Adjust Length menu\nThe current line length is "+(string)int1+"."+(string)int2+" m.";
    }else if(type=="laps"){
        menu=["3 Laps","4 Laps","5 Laps","Up","1 Lap","2 Laps"];
        s+="StartLine Laps Number menu\nLaps Number is "+llList2String(ls1,0);
    }else if(type=="time"){
        menu=["3 min.","4 min.","5 min.","Up","1 min.","2 min."];
        s+="StartLine CountDown menu\nThe timer is set to "+sec2hms((integer)llList2String(ls1,3));
    }else if(type=="racetime"){
        if(llList2String(ls1,2)=="7"){  //trial
            menu=["12 hours","18 hours","24 hours","3 hours","6 hours","9 hours","Up","1 hours","2 hours"];
            s+="StartLine Race Time menu\nThe trial time is set to "+sec2hms((integer)llList2String(ls1,10));
        }else{   //Trial Series
            menu=["90 minutes","105 minutes","120 minutes","45 minutes","60 minutes","75 minutes","Up","15 minutes","30 minutes"];
            s+="StartLine Race Time menu\nThe race time is set to "+sec2hms((integer)llList2String(ls1,10));
        }
    }else if(type=="minlaptime"){
        menu=["120 sec.","150 sec.","210 sec.","30 sec.","60 sec.","90 sec.","Up."];
        s+="StartLine MinLapTime menu\nThe minimum lap time is set to "+sec2hms((integer)llList2String(ls1,5));
    }else if(type=="direction"){
        string vs1;
        if(llList2String(ls1,1)=="1") vs1="+Y";
        else if(llList2String(ls1,1)=="-1") vs1="-Y";
        else vs1="Both";
        menu=["+Y","-Y","Both","Up"];
        s+="StartLine Direction menu\nThe Direction is set to "+vs1;
    }else if(type=="locktime"){
        if(llList2String(ls1,2)=="7" && isowner){ 
            menu=["150 min","180 min","Others...","60 min","90 min","120 min","Up.","30 min","45 min"];
            s+="StartLine Lock Time menu\nThe lock time is set to "+sec2hms((integer)llList2String(ls1,6));
        }else{
            menu=["150 min","180 min","  ","60 min","90 min","120 min","Up.","30 min","45 min"];
            s+="StartLine Lock Time menu\nThe lock time is set to "+sec2hms((integer)llList2String(ls1,6));
        }
    }else if(type=="mode"){
        //menu=["Trial Series","Start Training","Network","Match Race","Race Training","Multi Start","Up","Race","Trial"];
        menu=["Match Race","Start Training","Race Training","Race","Multi Start","Network","Up","Trial","Trial Series"];
        s+="StartLine Mode menu\nActual Mode is "+llList2String(ls1,15);
    }else if(type=="stages"){
        menu=["Bad 1m","Bad 45s","Bad 30s","Close 2m","Close 1m","Close 30s","Open 4m","Open 3m","Open 2m","Up"];
        s+="StartLine Stages menu\nMatch Stages Menu\nOpen Access: "+sec2hms((integer)llList2String(ls1,12))+
            "\nClose Access: "+sec2hms((integer)llList2String(ls1,13))+"\nBad Access: "+sec2hms((integer)llList2String(ls1,14));
    }else if(type=="startnum"){
        menu=["2 Starts","3 Starts","4 Starts","Up"];
        s+="StartLine Starts Number menu\nNumber of Starts: "+llList2String(ls1,11);
    }else if(type=="results"){  //results for multistarts
        integer n=(integer)llList2String(ls1,11);  //starts number
        if(n==2) menu=["Start.1","Start.2","  ","Up","Together","For Start"];
        if(n==3) menu=["Start.1","Start.2","Start.3","Up","Together","For Start"];
        if(n==4) menu=["Start.4","  ","  ","Start.1","Start.2","Start.3","Up","Together","For Start"];
        s+="StartLine Multistart Results menu";
    }else if(type=="starttime"){  //multistart display start time
        integer n=(integer)llList2String(ls1,11);  //starts number
        if(n==2) menu=["UP","Start-1","Start-2"];
        if(n==3) menu=["Start-1","Start-2","Start-3","UP"];
        if(n==4) menu=["Start-2","Start-3","Start-4","UP","Start-1"];
        s+="StartLine Multistart Display menu\nNumber of Starts: "+llList2String(ls1,11);
    }else if(type=="network"){
        integer vconnect=(integer)llList2String(ls1,18);
        string vs1="  ";
        if(vconnect) vs1="Net Invert";
        menu=["Up",vs1];
        s+="StartLine Network menu\nConnect: ";
        if(llList2String(ls1,18)=="1") s+=llList2String(ls1,19)+" sim";
        else s+="No Connect";
        s=s+"\nLink name: "+llList2String(ls1,17)+"\nLine type: ";
        string ss=llList2String(ls1,16);  //nettype
        if(ss=="S") s=s+"Start";
        else if(ss=="F") s=s+"Finish";
    }else if(type=="plugins"){
        menu=plugMenu+"Up.";
        s+="Startline Plugins menu";
    } 
    //llMessageLinked(LINK_THIS,1991,llDumpList2String(menu,","),llList2String(ls2,1)+s);   

    llListenRemove(dialogHandle); 
    dialogHandle = llListen(dialogChannel, "", "", "");
    llDialog((key)llList2String(ls2,1),s,menu,dialogChannel);
    if(menutimeout>0) llSetTimerEvent(menutimeout);
}

fDisplayMenu(key id)
{
    llMessageLinked(LINK_THIS,1901,"",id);   //call submenu function in startline script    
}

fSubMenu(string pmenu, key id)
{
    llMessageLinked(LINK_THIS,1902,pmenu,id);   //call submenu function in startline script
}

fOption(integer num, string str, key id)
{
    llMessageLinked(LINK_THIS, num, str, id);
}

fMode(string snum, string name, key id)
{
    llMessageLinked(LINK_THIS, 1903, snum+name, id);
}

fIsRun(integer num, string str, key id)   //Check if the line is stopped  num= 0-submenu  >0-option
{
    if(start_at>=0) llMessageLinked(LINK_THIS, 7100, "btnstartbad", id);
    else if(num==0) llMessageLinked(LINK_THIS,1902,str,id);    //== fSubMenu function
    else llMessageLinked(LINK_THIS, num, str, id);   //== fOption function
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
        llMessageLinked(LINK_THIS, 7100, "locked", (string)id+","+s+","+lockName+","+(string)lockTimeDef);
        llMessageLinked(LINK_THIS, 1000+lockType, "autolock", (string)id);
    } else {
        llMessageLinked(LINK_THIS, 1000+lockType, "autolock", "");
    }
}

//id,locktime;
fhudlock(string params)
{
    list l=llCSV2List(params);
    lockID=(key)llList2String(l,0);
    integer locktime=(integer)llList2String(l,1);
    string s="";
    integer index=llListFindList(owners,[lockID]);
    if(index>=0) lockType=2;
    else lockType=1;
    llMessageLinked(LINK_THIS, 1000+lockType, "locktype", "");
}


default 
{
    
    state_entry() 
    {
        owners=[llGetOwner()];
        getLinkNums();
        dialogChannel=-(integer)("0x"+llGetSubString((string)llGetKey(),-7,-1))-25;
        textboxChannel=-(integer)("0x"+llGetSubString((string)llGetKey(),-7,-1))-20;
    }
    
    listen(integer pchannel, string name, key id, string cmd) 
    {
        if (pchannel!=dialogChannel){ 
            if (pchannel==textboxChannel) {
                llListenRemove(textboxHandle);
                if (lineMode==7) {
                    integer vmin=(integer)cmd;
                    if (vmin>14 && vmin<1801) {
                        fOption(1928,"1"+(string)(60*vmin),id); return;    //Lock Time
                    } else {
                        llMessageLinked(LINK_THIS, 7100, "locktimebad", id);
                    }
                }
            }
            return;      //only menu channel is supported
        }
        
        llListenRemove(dialogHandle);
        if(!menuoutregion){
            if (llGetAgentSize(id)) {} else return;   //only if the message comes from an avatar outside the region
        }
        if (cmd == "Close Menu") return;
        string sname=llKey2Name(id);
        integer idxowner=llListFindList(owners,[id]);   //idxowner>=0 is owner 
            
        //first unlock options
        if(lockType>0){
            if(lockID==id){  //from menu option 
                if(lockType==1 && cmd=="Unlock"){ fOption(1950,"unlocksoft",id); return; }
                if(lockType==2 && cmd=="Unlock."){ fOption(1950,"unlockhard",id); return; }
            }else{  
                if(lockType==1 && cmd=="(Unlock)") fOption(1951,"unlocksoft",id);
                else if(lockType==2 && cmd=="(Unlock.)") fOption(1951,"unlockhard",id);
                else if(lockType==1) fOption(1951,"unlockbad",id);
                else fOption(1950,"unlockbad",id);
                return;
            }
        }
            
        if (cmd=="Up" || cmd=="<===") { fDisplayMenu(id); return; }
        if (cmd=="Up.") { fSubMenu("opt",id); return; }
        if (cmd == "Num.Laps...") { fIsRun(0, "laps", id); return; }
        if (cmd=="CountDown...") { 
            if(lineMode==3){
                if(start_at>=0){
                    if(llGetUnixTime()<=start_at){
                        llMessageLinked(LINK_THIS, 7100, "btnmstartbad", id);
                        return;
                    }
                }
                llMessageLinked(LINK_THIS,1902,"time",id);   //multistart mod countdown after start
            }else fIsRun(0, "time", id); 
            return; 
        }
        if (cmd == "MinLapTime...") { fIsRun(0, "minlaptime", id); return; } 
        if (cmd == "Mode...") { fIsRun(0, "mode", id); return; }
        if (cmd == "Direction...") { fIsRun(0, "direction", id); return; }
        if (cmd == "===>") { fSubMenu("opt",id); return; }
        if (cmd == "Help...") { fSubMenu("help",id); return; } 
        if (cmd == "Lock Time...") { fSubMenu("locktime",id); return; }
        if (cmd == "Return") { fSubMenu("linelength",id); return; }
        if (cmd == "Adjust") { fIsRun(0, "adjustline", id); return; }
        if (cmd == "Num.Starts...") { fIsRun(0, "startnum", id); return; }
        if (cmd == "Stages...") { fIsRun(0, "stages", id); return; }
        
        if (cmd=="Visible") { fOption(1911,"1",id); return; }   //visible
        if (cmd=="Invisible") { fOption(1911,"0",id); return; }   //invisible
        if (cmd=="Results" || cmd=="Results...")  { 
            if(resultsMode==3) fSubMenu("results",id);
            else fOption(1912,"",id);
            return;
        }        
        if (cmd=="Start")  {
            if(model==1 && startRez==0) llMessageLinked(LINK_THIS, 7100, "flynobuild", id);    //First you have to press the 'Build Line' option     
            else fIsRun(1913, "", id);
            return;
        }            
        if (cmd == "Finish/Restart") { fOption(1914,"",id); return; }
        if (cmd == "Reset") { fOption(1915,"",id); return; }
        if(cmd=="Save Default") { fOption(1916,"",id); return; } 
        if (cmd == "Help.") { 
            llGiveInventory(id,"StartLine readme"); 
            fOption(1917,"0",id); 
            return; 
        }
        if (cmd == "License") { 
            llGiveInventory(id,"StartLine License");
            fOption(1917,"1",id);                            
            return; 
        }
        if (cmd == "+Y" || cmd=="-Y" || cmd=="Both") { fOption(1918,cmd,id); return; }  //direction
        if (cmd == "Lock") { 
            if (idxowner>=0) fOption(1919,"1",id);
            else fOption(1919,"0",id);
            return; 
        }if (cmd == "Hard Lock") { 
            if (idxowner>=0) fOption(1920,"1",id);
            else fOption(1920,"0",id);
            return; 
        }if (cmd == "Load Default") { fIsRun(1921, "", id); return; }
        if (cmd == "Line Length") { fIsRun(1922, "", id); return; }
        if (cmd=="-" || cmd=="--" || cmd=="---" || cmd=="+" || cmd=="++" || cmd=="+++") {           //1923 free**
            float inc=0;
            if(cmd=="-") inc=-0.5;
            else if(cmd=="--") inc=-1.0;
            else if(cmd=="---") inc=-3.0;
            else if(cmd=="+") inc=0.5;
            else if(cmd=="++") inc=1.0;
            else if(cmd=="+++") inc=3.0;
            llMessageLinked(LINK_THIS, 1001, "length", (string)id+(string)inc);   //1001 adjust
            llSleep(1);
            fSubMenu("adjustline",id);
            return;
        }
        if (cmd == "Race Time...") { fIsRun(0, "racetime", id); return; }                                                                //1924 free** 
        if (cmd == "Race") { fMode("0", cmd, id); return; }
        if (cmd == "Match Race") { fMode("1", cmd, id); return; }
        if (cmd == "Trial Series") { fMode("2", cmd, id); return; }
        if (cmd == "Multi Start") { fMode("3", cmd, id); return; }
        if (cmd == "Race Training") { fMode("4", cmd, id); return; }
        if (cmd == "Start Training") { fMode("5", cmd, id); return; }
        if (cmd == "Network") { fMode("6", cmd, id); return; }
        if (cmd == "Trial") { fMode("7", cmd, id); return; } 
        if (llGetSubString(cmd,2,4)=="Lap") { fOption(1925,llGetSubString(cmd,0,0),id); return; }   //NumLaps
        if (llGetSubString(cmd,2,5)=="min.") { fOption(1926,(string)(60*(integer)llGetSubString(cmd,0,0)),id); return; }  //Countdown Time
        if (llGetSubString(cmd,-4,-1)=="sec.") { fOption(1927,llGetSubString(cmd,0,-5),id); return; }   //MinLapTime
        if (llGetSubString(cmd,-3,-1)=="min") { fOption(1928,"0"+(string)(60*(integer)llGetSubString(cmd,0,-4)),id); return; }   //Lock Time
        if (llGetSubString(cmd,-2,-1) == "m.") {            //line length
            llMessageLinked(LINK_THIS, 1000, "length", (string)id+llGetSubString(cmd,0,2));   
            fSubMenu("linelength",id);
            return; 
        }     

        if(lineMode==1){   //match
            if (llGetSubString(cmd,0,3)=="Open") { fOption(1930,(string)(60*(integer)llGetSubString(cmd,-3,-2)),id); return; } //match open 
            if (llGetSubString(cmd,0,4)=="Close") { fOption(1931,llGetSubString(cmd,-3,-2),id); return; } //match close    
            if (llGetSubString(cmd,0,2)=="Bad") { fOption(1932,llGetSubString(cmd,-3,-2),id); return; } //match bad 
        }
        if(lineMode==2){   //Trial Series
            if (llGetSubString(cmd,-8,-1)==" minutes") { fOption(1933,llGetSubString(cmd,0,-8),id); return; } //race time    Trial Series
        }
        if(lineMode==3){  //multistart
            if (llGetSubString(cmd,-6,-1)=="Starts") { fOption(1934,llGetSubString(cmd,0,0),id); return; }   //starts number
            if (llGetSubString(cmd,0,5)=="Start ") { fOption(1935,llGetSubString(cmd,6,6),id); return; }
            if (llGetSubString(cmd,0,7)=="PreStart") { fOption(1936,llGetSubString(cmd,9,9),id); return; }
            if (cmd=="Starts Time...") { fOption(1937,"",id); return; }
            if (llGetSubString(cmd,0,5)=="Start-") { fOption(1947,llGetSubString(cmd,6,6),id); return; }
        }
        if(resultsMode==3){  //multistart
            if (cmd=="Together" || cmd=="For Start" || llGetSubString(cmd,0,5)=="Start.") { fOption(1938,llToLower(cmd),id); return; }  
        }
        if(lineMode==6){   //network
            if (cmd == "Network...") { fIsRun(0, "network", id); return; }
            if(cmd=="Net Invert"){
                if(start_at<0) fOption(1948,"",id); 
                else llMessageLinked(LINK_ALL_OTHERS,7100,"btnstartbad",id);
                return;
            }
        }
        if(lineMode==7){   //trail mode
            if (llGetSubString(cmd,-6,-1)==" hours") { fOption(1945,llGetSubString(cmd,0,-6),id); return; }   //race time    trail mode
            if (cmd == "Others...") {
                llListenRemove(textboxHandle);
                textboxHandle = llListen(textboxChannel, "", "", "");
                llTextBox(id ,"Enter time in minutes (30min - 1800min):" , textboxChannel);
                if(menutimeout>0) llSetTimerEvent(menutimeout);
                return;
            }
            
        }
        if (model==1) {
            if (cmd=="Build Line") { 
                startRez=1;
                fOption(1981,"1",id);
            }else if (cmd=="Destroy Line") {
                startRez=0;
                fOption(1981,"0",id);
            }
        }
        if (cmd == "Admin Menu...") {
            if (llListFindList(owners, [id])>=0) {
                fSubMenu("admin",id);
                return;
            } else {
                llMessageLinked(LINK_THIS, 7100, "ownermenubad",id);
                fSubMenu("opt",id);
            }
            return;
        }
        if (nPlugMenu>0) {
            if (cmd == "Plugins") {
                fSubMenu("plugins",id);
            }else if(llGetSubString(cmd,0,2)=="PL-"){ 
                fOption(1946,llGetSubString(cmd,3,-1),id);    //call plugin menu
            }
            return;
        }
        if(cmd=="Test"){   //only for test
            fOption(1990,"","");
            return;
        }
    }
    
    link_message(integer sender_num, integer num, string str, key data) 
    {
        if (num>=0 && num<1000) {   //link messages for all scripts
            if (str=="start") {
                start_at=(integer)((string)data);
                resultsMode=lineMode;
                return;
            }        
            if (str=="restart") {
                start_at=-1;
                return;
            }
            if (str=="setopt") {
                list l=llCSV2List(data);
                lineMode=(integer)llList2String(l,5);
                return;
            }
            if (str=="config") {
                list l=llCSV2List(data); 
                dbg=(integer)llList2String(l,8);
                menuoutregion=(integer)llList2String(l,11);
                menutimeout=(integer)llList2String(l,13);
                model=(integer)llList2String(l,14);
                if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory()); 
            }
            if (model==1){
                if(str=="build") startRez=1;
                else if(str=="destroy") startRez=0;
            }
            return;
        }
        if (num<4000 || num>4999) return;   //Menu messages num between 4000 and 4999
        
        if(num>=4990){
            if(num==4991) SubMenu(str,(string)data);    
            else if(num==4992) DisplayMenu(str,(string)data);
            return;
        }
        
        if (str=="lockvars") {  //update locked vars from startline script
            lockType=num-4000;
            lockID=data;
        }

        if (str=="admins") {
            list l=llCSV2List(data);
            integer n=llGetListLength(l);
            key ow;
            while (n>0) { 
                ow=(key)llList2String(l,n-1);
                if(ow) owners+=ow;
                n--;
            }
            return;
        }
        if (str=="plugmenu") {
            plugMenu+=data;
            nPlugMenu=llGetListLength(plugMenu);
            return;
        }
        if (str=="netstopw") {
            swWaiting=0;
        }
        if(str=="autolock"){
            fautolock((string)data);
            return;
        }
        if(str=="hudlock"){
            fhudlock((string)data);
            return;
        }
    }
    
    timer()
    {
        llSetTimerEvent(0.0);
        llListenRemove(dialogHandle); 
        llListenRemove(textboxHandle);
    }    
}





