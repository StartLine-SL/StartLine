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
//this scripts send llInstantMessage, this command sleep script 2"
integer dbg;
integer model;

dbgSay(string text)
{
    if(dbg!=0){ 
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
}

sendMsg(key id, string msg)
{
    if (llGetAgentSize(id)) llRegionSayTo(id,0,msg);
    else llInstantMessage(id,msg);   
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

default {
    link_message(integer sender_num,integer num,string str,key id) {
        if(num>=0 && num<1000){         
            if (str=="config") {
                list l=llCSV2List(id); 
                dbg=(integer)llList2String(l,8);
                dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                return;
            }
        }
        if(num<7000 || num>7999) return;   //messages num between 7000 and 7999
        
        
        if (num==7000) {   //llMessageLinked(LINK_THIS,7000,message,id);
            llInstantMessage(id,str);
        } else if(num==7001) { //llMessageLinked(LINK_THIS,7001,message,id); 
            //send regionsayto or llinstant depending on whether the avatar is in the region
            if (llGetAgentSize(id)) llRegionSayTo(id,0,str);
            else llInstantMessage(id,str);
        } else if(num==7002) { //llMessageLinked(LINK_THIS,7002,"code",data);  code=code message
            if (str=="unlockedby" || str=="unlockedow") {
                list l=llCSV2List(id);  //data=(string)lockID+","+linename+","+(string)id
                vector pos=llGetPos();
                string urlpos="http://maps.secondlife.com/secondlife/"+llEscapeURL(llGetRegionName())+"/"+(string)llRound(pos.x)+"/"+(string)llRound(pos.y)+"/"+(string)llRound(pos.z);
                string s="";
                if (str=="unlockedow") s="the owner ";
                llInstantMessage((key)llList2String(l,0), "The "+llList2String(l,1)+" "+urlpos+" has been unlocked by "+s+llKey2Name((key)llList2String(l,2))+" secondlife:///app/agent/"+llList2String(l,2)+"/im");
            }
        } else if (num==7005) {    //network send connection  from startline script
            //str=nettype,id,llGetRegionName(),netLineUrl
            llEmail((string)id+"@lsl.secondlife.com", "netConnectReq", str);
/*
        } else if (num==7006) {    //network send connection received  from send results script
            string address=str+"@lsl.secondlife.com";
            string subject="netConnectAns";
            string message=(string)id;
            llEmail(address, subject, message);
        } else if (num==7007) {    //network send start  from send results script
            string address=str+"@lsl.secondlife.com";
            string subject="netRStart";
            string message=(string)id;
            llEmail(address, subject, message);
        } else if (num==7008) {    //network send data start to end
            llOwnerSay("msg enviando email netRData");
            string address=str+"@lsl.secondlife.com";
            string subject="netRData";
            string message=(string)id;
            llEmail(address, subject, message);
        } else if (num==7009) {    //network send restart to other line
            string address=str+"@lsl.secondlife.com";
            string subject="netRestart";
            string message=(string)id;
            llEmail(address, subject, message);
*/            
        } else if (num>=7100 && num<7200) { //llMessageLinked(LINK_THIS,71NN,data,id);  NN number message 
            //send regionsayto or llinstant depending on whether the avatar is in the region
            string s;
            if(str=="btnstartbad") sendMsg(id,"Race is running, press Finish/Restart button");
            else if(str=="btnmstartbad") sendMsg(id,"Start is running, wait finish it");
            else if(str=="loaddefault") sendMsg(id,"Default options have been loaded");
            else if(str=="changedirection") sendMsg((key)llGetSubString(id,0,35),"new direction: "+llGetSubString(id,37,-1));         
            else if(str=="unlocked") sendMsg(id, "The line is now unlocked");
            else if(str=="locked"){ 
                list l=llCSV2List(id);
                string s;
                if(llList2String(l,1)=="1") s="hard locked";
                else s="locked";
                s="StartLine "+s+" by "+llList2String(l,2)+" for "+sec2hms((integer)llList2String(l,3));
                sendMsg((key)llList2String(l,0),s); 
            }else if(str=="lockednews"){
                list l=llCSV2List(id); 
                string s="This line is locked by administrator "+llList2String(l,1)+" for "+sec2hms((integer)llList2String(l,2))+".";
                s+=" Please contact to him/her secondlife:///app/agent/"+llList2String(l,3)+"/im if you need to override the lock";
                sendMsg((key)llList2String(l,0),s); 
            }else if(str=="numlaps") sendMsg(id,"New number of race laps: "+(string)(num-7100));
            else if(str=="countdowntime") sendMsg((key)llGetSubString(id,0,35),"Countdown time: "+sec2hms((integer)llGetSubString(id,36,-1)));
            else if(str=="minlaptime") sendMsg((key)llGetSubString(id,0,35),"New minimum lap time: "+sec2hms((integer)llGetSubString(id,36,-1)));
            else if(str=="resetline") sendMsg(id,"Reset line");
            else if(str=="save") sendMsg(id,"Options have been saved"); 
            else if(str=="ownermenubad") sendMsg(id,"This option is for owners");
            else if(str=="locktime") sendMsg((key)llGetSubString(id,0,35),"Lock time: "+sec2hms((integer)llGetSubString(id,36,-1)));
            else if(str=="lock"){
                list l=llCSV2List(id); 
                sendMsg((key)llList2String(l,0),"StartLine locked by "+llList2String(l,1)+" for "+sec2hms((integer)llList2String(l,2))); 
            }else if(str=="hardlock"){
                list l=llCSV2List(id); 
                sendMsg((key)llList2String(l,0),"StartLine hard locked by "+llList2String(l,1)+" for "+sec2hms((integer)llList2String(l,2))); 
            }else if(str=="islocked") sendMsg(id,"This line is locked, unlock it first before locking it again");
            else if(str=="ishardlocked") sendMsg(id,"This line is locked, unlock it first before locking it again");  
            else if(str=="racetime") sendMsg((key)llGetSubString(id,0,35),"Race time: "+sec2hms((integer)llGetSubString(id,36,-1)));
            else if(str=="msgversion"){
                list l=llCSV2List(id);   //id=linename,version
                llSay(0,llList2String(l,0)+" version "+llList2String(l,1));
                llSay(0,llList2String(l,0)+" Copyright (C) see help file");
                llSay(0,llList2String(l,0)+" comes with ABSOLUTELY NO WARRANTY");
                llSay(0,llList2String(l,0)+" is free software (GPL-3.0 or later). See Licence file");
            }else if(str=="startbad") llSay(0,(string)id+" does not work because not started properly. Reset the line");
            else if(str=="ready") llSay(0,(string)id+" is ready");
            else if(str=="mode"){ 
                list l=llCSV2List(id);   //id=mode,modename,id
                sendMsg((key)llList2String(l,2),llList2String(l,1)+" mode has been set");
            }else if(str=="matchway") sendMsg(id,"The match mode needs to define a start direction");
            else if(str=="startnum") sendMsg((key)llGetSubString(id,0,35),"Number of Starts: "+llGetSubString(id,36,-1));
            else if(str=="matchopen") sendMsg((key)llGetSubString(id,0,35),"Match Open Access Time: "+llGetSubString(id,36,-1));
            else if(str=="matchclose") sendMsg((key)llGetSubString(id,0,35),"Match Close Access Time: "+llGetSubString(id,36,-1));
            else if(str=="matchbad") sendMsg((key)llGetSubString(id,0,35),"Match Bad Access Time: "+llGetSubString(id,36,-1));
            else if(str=="netlink") sendMsg((key)llGetSubString(id,0,35),"StartLine to connect: "+llGetSubString(id,36,-1));
            else if(str=="nettypebad") sendMsg(id,"You have to select if it's start or end");
            else if(str=="netlinkbad") sendMsg(id,"You have to select the line with which you are going to connect");
            else if(str=="netcodebad") sendMsg(id,"The code you have entered is not valid");
            else if(str=="netcodeok") sendMsg(id,"Network Link Code OK");
            else if(str=="netkeybad") sendMsg(id,"Invalid code");
            else if(str=="waiting") sendMsg(id,"Waiting for connection");
            else if(str=="stopwait") sendMsg(id,"Stopped waiting");
            else if(str=="connectrec") sendMsg(id,"Connection received in the Finish Line");
            else if(str=="connectok") sendMsg(id,"Connection established");
            else if(str=="visible") if(num-7100==1) sendMsg(id,"The line is visible"); else sendMsg(id,"The line is invisible");
            else if(str=="linelength") sendMsg((key)llGetSubString(id,0,35),"The new length is "+llGetSubString(id,36,-1)+"m.");
            else if(str=="flynobuild") sendMsg(id,"First you have to press the 'Build Line' option");
            else if(str=="locktimebad") sendMsg(id,"Invalid Lock Time");
            else llOwnerSay("*************************************************** error 7100 msg "+str+"  "+(string)id);
        }
    }
}















