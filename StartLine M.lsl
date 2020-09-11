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
//About this script - StartLine M

integer dbg=0;

//Global Variables/////////////////////////////////////////////
//settings prog
integer minTime=30;
integer raceNumLaps=1;
integer ways=0; //-1 -Y; +1 +Y; 0 Both

integer channel;
integer pingChannel; //the channel to listen for chronometers and others objects, It is loaded from the StartLine Aux script
integer region_relay_channel=0;  //It is loaded from the StartLine Aux script
integer setDName;
integer laptimessay;

integer lapNum=0;   //this is the number of laps in the race
key  lockID=NULL_KEY;
integer locktime=0;
integer SEND=0;

//Detector Block
list mute0=[];
list mute1=[];
list mute2=[];
list mute3=[]; 
integer muteIndex=0;

//Register Block
// It is triggered by messages from the detector
//integer i;
//string id;
integer idStrIdx;
list lapList=[];
list timeList=[];
list ownerList=[];
list allLapsList=[];
list finishList=[];
list nameList=[];

list tmp;
string name;

integer winnerFlag=FALSE;
integer runnerupFlag=FALSE;
key ownerKey;
string ownerName;

//Other Block
integer start_at;           // The point in time (as unix time) when the start is planned / has been. -1 if not active
integer last_tick;          // The last second (Unix time) the counter worked
integer started=FALSE;

list cmd_list;
string cmd;

integer lock=FALSE;
integer swRun;
//END Global Variables/////////////////////////////////////////////

//Blocks/////////////////////////////////////////////
//Detector Block-> 
integer isMuted(key id) 
{
    list idl=[id];
    integer idx=0;
    
    idx=llListFindList(mute0, idl);
    if (idx==-1) {
        idx=llListFindList(mute1, idl);
        if (idx==-1) {
            idx=llListFindList(mute2, idl);
            if (idx==-1) {
                idx=llListFindList(mute3, idl);
            }
        }
    }
        
    if(muteIndex==0)
        mute0=idl+mute0;
    else if(muteIndex==1)
        mute1=idl+mute1;
    else if(muteIndex==2)
        mute2=idl+mute2;
    else if(muteIndex==3)
        mute3=idl+mute3;
    
    if(idx>=0) 
        return 1;
    else
        return 0;        
}

//checks if boat is crossing in right direction (in direction of root local y-axis)
integer fwdCrossing(vector v) {
    if (ways==0)
        return TRUE;
        
    float projection=llRot2Left(llGetRot())*v;
    if (projection>0 && ways==1) 
        return TRUE;
    if (projection<0 && ways==-1) 
        return TRUE;
    else 
        return FALSE;
}

handle_collision(integer num, integer diff, integer now) {

    dbgSay("Handling collision for object "+(string)num);
    
    string name=llDetectedName(num);    //boat name + boat id
    key ownerKey=llDetectedOwner(num);  //owner boat key
    string ownerName=llKey2Name(ownerKey);  //legacy name owner boat
    string ownerDName; 
    if (setDName==1) ownerDName=llGetDisplayName(ownerKey);   //display name owner boat
    else ownerDName=ownerName;    //legacy name
    integer idStrIdx=llSubStringIndex(name,"#");  //takos that are registerd will have "#nn" in their name

    if (idStrIdx<0) {   // Silently ignore
        dbgSay("Collision with non-id obect owned by "+ownerName);
        return;     //"*****"
    }

    string strType=llGetSubString(name, 0, idStrIdx-1);  //boat name
    string strId  =llGetSubString(name, idStrIdx+1, -1);  //boat id
    vector vel=llDetectedVel(num);
    
    if (isMuted(ownerKey)) {
        dbgSay("Muted-->ignore");
        return;
    }

    if (!fwdCrossing(vel)) {
        string msg=ownerDName+"  ID"+strId+" wrong way!";
        llShout(0,"/me : "+msg); 
        if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
        llShout(channel,"badir|"+(string)ownerKey+"|"+strId+"|"+(string)ways);
        return;
    }
    
    if (diff<0) { //over early
        string msg=ownerDName+"  ID"+strId+" is over early!  Go around and recross!";
        llShout(0,"/me : "+msg); 
        if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
        llShout(channel,"early|"+(string)ownerKey+"|"+strId+"|"+(string)diff+"|"+(string)now);
        return;
    }
    
    string s="crossing|"+ownerDName+"|"+ownerName+"|"+(string)ownerKey+"|"+(string)diff+"|"+(string)now+"|"+strType+"|"+strId;
    llMessageLinked(LINK_THIS, 0, s, NULL_KEY);
    
    return;
}

handle_crossing(string list_argument) {
    list parse=llParseString2List(list_argument, ["|"], []);
    string ownerDName=llList2String(parse,1);   //boat owner display name
    string ownerName=llList2String(parse,2);    //boat owner name
    key ownerKey=llList2Key(parse,3);           //boat owner key
    integer raceTimeRel=llList2Integer(parse,4);
    integer raceTimeAbs=llList2Integer(parse,5);
    string boatType=llList2String(parse,6);
    string raceID=llList2String(parse,7);
    integer listIdx=llListFindList(ownerList,[(string)ownerKey+raceID]);
    integer lapTime;
    string msg;
    
    if (listIdx==-1) { //not in list... will add to list if not over early
        dbgSay("Didn't find racer "+ownerName+" in list. Adding him/her");
        addRacer(ownerName, ownerKey, raceTimeRel, raceTimeAbs, boatType, raceID);
        msg=ownerDName+"  ID"+raceID+" starts at time "+sec2hhmmss(raceTimeRel);
        llShout(0,"/me : "+msg); 
        if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
        llShout(channel,"avsta|"+(string)ownerKey+"|"+raceID+"|"+(string)raceTimeRel);
    } else { //in the list, so they must have started already
        dbgSay("Found racer "+ownerName+" in list. Checking for lap time.");
        
        if (llFabs(llList2Float(timeList,listIdx)-raceTimeRel)>minTime) { //is time dif > min lap time?
            dbgSay("Racer "+ownerName+" is over min lap time, checking for lap numbers");
            lapNum=llList2Integer(lapList,listIdx);
            ++lapNum;
            
            if (lapNum<=raceNumLaps) {
                lapList=llListReplaceList(lapList,[lapNum],listIdx,listIdx); //update lap number for this racer
                lapTime=llList2Integer(timeList,listIdx);
                lapTime=raceTimeRel-lapTime;
                timeList=llListReplaceList(timeList,[raceTimeRel],listIdx,listIdx); //update race time for this racer
                //add 
                allLapsList+=["r"+(string)listIdx,lapNum,lapTime];
                if (lapNum==raceNumLaps) {
                    if (llListFindList(finishList,[listIdx])==-1) finishList+=listIdx;
                    string snumorder=(string)llGetListLength(finishList);
                    if (!winnerFlag) {
                        msg="Winner: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                        winnerFlag=TRUE;
                    } else if (!runnerupFlag) {
                        msg="Runner up: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                        runnerupFlag=TRUE;
                    } else {
                        msg="#"+snumorder+": "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                    }
                    if(laptimessay){
                        string s="\nLap times";
                        integer idx;
                        integer y;
                        for (y=0;y<=raceNumLaps;++y) {
                            idx=llListFindList(allLapsList,["r"+(string)listIdx,y]);
                            if (idx>=0) {
                                if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                                else if(y==raceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                                else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                            }
                        }
                        msg+=s;
                    }
                    llShout(channel,"avend|"+(string)ownerKey+"|"+raceID+"|"+snumorder+"|"+(string)lapTime+"|"+(string)raceTimeRel);
                } else {
                    msg=ownerDName+"  ID"+raceID+": "+(string)lapNum+" lap(s) completed at time "+sec2hhmmss(lapTime)+"/"+sec2hhmmss(raceTimeRel);
                    llShout(channel,"avlap|"+(string)ownerKey+"|"+raceID+"|"+(string)lapNum+"|"+(string)lapTime+"|"+(string)raceTimeRel);
                }
                llShout(0,"/me : "+msg); 
                if(region_relay_channel!=0) llRegionSay(region_relay_channel, msg);
            } else {
                dbgSay("Racer "+ownerName+" is already finished");
            }
        } else {
            dbgSay("Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.");
        }
    }
}

//<-Detector Block

//Register Block->
//when racer crosses line for the first time (after the start), add place to lists
addRacer(string ownername, key owner, integer startTimeRel, integer startTimeAbs, string boatType, string raceID) {
    integer listIdx=llGetListLength(ownerList);
    lapList+=[0];
    timeList+=[startTimeRel];
    ownerList+=[(string)owner+raceID];
    nameList+=ownername+" "+raceID;
    allLapsList+=["r"+(string)listIdx,0,startTimeRel];
    //llMessageLinked(LINK_THIS, 9, "start|"+name+"|"+(string)startTimeRel+"|"+(string)startTimeAbs+"|"+boatType+"|"+raceID, NULL_KEY);
}
//<-Register Block

dbgSay(string text)
{
    if (dbg>0) llSay(0, text);
}


//convert integer time in seconds to "hh:mm:ss" string
string sec2hhmmss(integer seconds) {
    string hms;
    if (seconds<0) hms="-";
    else hms="";
    seconds=llAbs(seconds);
    integer hr=(integer)(seconds/3600);
    integer rest=seconds-hr*3600;
    integer mn=(integer)(rest/60);
    integer sc=rest-mn*60;
    hms+=(string)hr+":";
    if (mn<10) hms+="0"+(string)mn+":";
    else hms+=(string)mn+":";
    if (sc<10) hms+="0"+(string)sc;
    else hms+=(string)sc;
    return hms;
} 

Restart()
{
    swRun=0;
    llSetTimerEvent(0);
    start_at=-1;
}

Start(integer argument)
{
    start_at=argument;
    //Register reset
    lapList=[];
    timeList=[];
    ownerList=[];
    allLapsList=[];
    nameList=[];
    finishList=[];
    winnerFlag=FALSE;
    runnerupFlag=FALSE;
    mute0=[]; 
    mute1=[];
    mute2=[];
    mute3=[];
    muteIndex=0;    
    //end Register
    swRun=1;
    llSetTimerEvent(1.0);
}

sayResults(integer swSend,string results)
{
    if (swSend==0) llShout(0,"/me :"+results);
    else if(SEND>0) llMessageLinked(LINK_ALL_OTHERS, 0, "results|"+results, NULL_KEY); 
    else llShout(0,"/me :"+results);
}

Results()
{
    string results="\nRace Results:";
    string line;
    key ownerKey;
    integer i;
    integer y;
    integer listIdx;
    integer len=llGetListLength(finishList);
    integer len2=llGetListLength(lapList);
    integer swSend=0;
    for (i=0;i<len;++i) {    //finished racers
        listIdx=llList2Integer(finishList,i);
        line="\n"+(string)(i+1)+": ";
        line+=llList2String(nameList,listIdx)+" - Race Time: ";
        line+=sec2hhmmss(llList2Integer(timeList,listIdx));
        if(llStringLength(results+line)>1000) {
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    len2=llGetListLength(lapList);
    for (i=0;i<len2;++i) {   //all racers
        if (llList2Integer(lapList,i)<raceNumLaps) {
            line="\n"+(string)(len+i)+": ";
            line+=llList2String(nameList,i)+" - ";
            line+="Not Finished";
            if(llStringLength(results+line)>1000) {
                sayResults(swSend,results);
                swSend=1;
                results=line;
            }else{
                results+=line;
            }
        }
    }
    sayResults(swSend,results+"\n");
    swSend=1;
    //lap times
    results="\nLap Times:";
    list lap;
    string s;
    string lapStr;
    string timeStr;
    integer idx;
    for (i=0;i<len;++i) {    //finished racers
        listIdx=llList2Integer(finishList,i);
        line="\n"+llList2String(nameList,listIdx);
        s="";
        for (y=0;y<=raceNumLaps;++y) {
            idx=llListFindList(allLapsList,["r"+(string)listIdx,y]);
            if (idx>=0) {
                if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                else if(y==raceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
            }
        }
        line+=s;
        if(llStringLength(results+line)>1000) {
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    for (i=0;i<len2;++i) {   //all the racers
        if (llList2Integer(lapList,i)<raceNumLaps) {
            line="\n"+llList2String(nameList,i);
            s="";
            for (y=0;y<=raceNumLaps;++y) {
                idx=llListFindList(allLapsList,["r"+(string)i,y]);
                if (idx>=0) {
                    if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                    else if(y==raceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                    else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                } else {
                    if (y==0) s+=" - Start: Not Finished";
                    else if(y==raceNumLaps)  s+=" - Last lap: Not Finished";
                    else s+=" - Lap "+(string)y+": Not Finished";
                    y=raceNumLaps;
                }
            }
            line+=s;
            if(llStringLength(results+line)>1000) {
                sayResults(swSend,results);
                swSend=1;
                results=line;
            }else{
                results+=line;
            }
        }
    }
    sayResults(swSend,results+"\n");
    if (dbg==2){
        llSleep(0.5);
        dbgSay("-");
        dbgSay("lapList="+llDumpList2String(lapList, "|"));
        dbgSay("timeList="+llDumpList2String(timeList, "|"));
        dbgSay("ownerList="+llDumpList2String(ownerList, "|"));
        dbgSay("nameList="+llDumpList2String(nameList, "|"));
        dbgSay("allLapsList="+llDumpList2String(allLapsList, "|"));
        dbgSay("finishList="+llDumpList2String(finishList, "|"));
        
        dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    }
}

inicio()
{
        dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
} 

default 
{
    state_entry()
    {
        start_at=-1;
        swRun=0;
        llSetTimerEvent(0.0);
    }
    
    collision_start(integer num) {
        dbgSay("Collision detected");
        if (start_at==-1) {
            dbgSay("I'm not on. leaving premature");
            return;
        }

        integer now=llGetUnixTime();    // Get the time as early as possible
        integer diff=now-start_at;
        dbgSay("Collision detected with "+(string)num+" objects at "+(string)diff);
        integer i;
        for (i=0;i<num;++i) {
            handle_collision(i, diff, now);
        } // for
    }
    
    link_message(integer sender_num, integer num, string str, key id)
    {
        if (llGetSubString(str,0,7)=="crossing") {
            handle_crossing(str);
            return;
        }
        if (str=="start") {
            Start(num);
            return;
        }
        if (str=="restart") {
            Restart();
            return;
        }
        if (str=="results") {
            Results();
            return;
        }
        if (llGetSubString(str,0,5)=="setopt") {
            list parse=llParseString2List(str, ["|"], []);
            setDName=(integer)llList2String(parse,1);
            minTime=(integer)llList2String(parse,2);
            ways=(integer)llList2String(parse,3);
            raceNumLaps=(integer)llList2String(parse,4);
            SEND=(integer)llList2String(parse,4);
            return;
        }
        if (llGetSubString(str,0,5)=="config") {
            list l=llParseString2List(str,["|"],[]); 
            channel=(integer)llList2String(l,1);
            pingChannel=(integer)llList2String(l,2);
            dbg=(integer)llList2String(l,9);
            region_relay_channel=(integer)llList2String(l,10);
            laptimessay=(integer)llList2String(l,13);
            inicio();
            return;
        }                
    }

    timer()
    {
        muteIndex++;
        if(muteIndex==1)
            mute1=[];
        else if(muteIndex==2)
            mute2=[];
        else if(muteIndex==3)
            mute3=[];
        else {
            muteIndex=0;
            mute0=[];
        }
    }

    on_rez(integer whocares)
    {
        llResetScript();
    }
}