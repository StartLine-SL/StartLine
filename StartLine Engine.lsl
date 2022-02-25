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
integer model;

//Global Variables///////////////////////////////////////////// 
//settings prog
integer minTime=0;
integer raceNumLaps=1;
integer ways=0; //-1 -Y; +1 +Y; 0 Both
list calls=[0];  //list of calls  first element is 0  call num 22000


integer channel;
integer pingChannel;
integer region_relay_channel=0;
integer setDName;


key  lockID=NULL_KEY;
integer locktime=0;

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
list finishList=[];
list finishList1=[];
list finishList2=[];
list finishList3=[];
list finishList4=[];
list nameList=[];
list startList=[];
//list allLapsList=[];

list tmp;
string name;
integer lineMode;
integer resultsMode;
integer time1;
integer time2;
integer time3;
string sailor1;
string sailor2;
string matchName1;
string matchName2;
integer nNetSend;
integer netIndex;
integer netIndex2;
string nettype;

integer winnerFlag=FALSE;
list lwinnerFlag;
integer runnerupFlag=FALSE;
list lrunnerupFlag;
key ownerKey;
string ownerName;

integer start_at;           // The point in time (as unix time) when the start is planned / has been. -1 if not active
integer last_tick;          // The last second (Unix time) the counter worked
integer started=FALSE;
integer startN;             //multistart actual start
integer startat1;           //multistart start 1 start time
integer startat2;
integer startat3;
integer startat4;

list cmd_list;
string cmd;

integer lock=FALSE;
integer swRun;
integer sw;
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
    
    if(idx>=0) return 1;
    else return 0;        
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

    if(dbg!=0) dbgSay("Handling collision for object "+(string)num);

    key ownerKey=llDetectedOwner(num);  //owner boat key
    if (isMuted(ownerKey)) {
        if(dbg!=0) dbgSay("Muted-->ignore");
        return;
    }
    
    string name=llDetectedName(num);    //boat name + boat id
    string ownerName=llKey2Name(ownerKey);  //legacy name owner boat
    string ownerDName; 
    if (setDName==1) ownerDName=llGetDisplayName(ownerKey);   //display name owner boat
    else ownerDName=ownerName;    //legacy name
    integer idStrIdx=llSubStringIndex(name,"#");  //takos that are registerd will have "#nn" in their name

    if (idStrIdx<0) {   //non ID boat.  Silently ignore
        if(dbg!=0) dbgSay("Collision with non-id obect owned by "+ownerName);
        return;     //*****
    }

    string strType=llGetSubString(name, 0, idStrIdx-1);  //boat name
    string strId  =llGetSubString(name, idStrIdx+1, idStrIdx+6);  //boat id 6 characters same Hay line and SLSA protocol
    vector vel=llDetectedVel(num);
    
    if(!fwdCrossing(vel)){   //cross in the opposite direction
        if(lineMode==1){  //match mode
            if(diff<0){   //if pre start
                diff=llAbs(diff);
                if(sailor1!="" && sailor2!="") return;
                if(diff>time3) llMessageLinked(LINK_THIS,7213,"sendmsg2",ownerDName+"  ID"+strId);   //has crossed early!
                else if(diff<=time1) llMessageLinked(LINK_THIS,7214,"sendmsg2",ownerDName+"  ID"+strId);  //has entered the pre-start zone too late you are DSQ
                else{
                    if(sailor1!=(string)ownerKey+strId && sailor2!=(string)ownerKey+strId){
                        if(diff>time2) llMessageLinked(LINK_THIS,7215,"sendmsg2",ownerDName+"  ID"+strId);   //has entered the pre-start zone
                        else if(diff>time1) llMessageLinked(LINK_THIS,7216,"sendmsg2",ownerDName+"  ID"+strId);    //has entered the pre-start zone out of time
                        if(sailor1==""){ 
                            sailor1=(string)ownerKey+strId;
                            matchName1=ownerDName+"  ID"+strId;
                        }else if(sailor2==""){ 
                            sailor2=(string)ownerKey+strId;
                            matchName2=ownerDName+"  ID"+strId;
                        }
                        llMessageLinked(LINK_THIS,7200,"chanmsg","avmch,"+(string)ownerKey+","+strId+","+(string)sw+","+(string)now); 
                        if(sailor1!="" && sailor2!="") llMessageLinked(LINK_THIS,7212,"sendmsg2",matchName1+","+matchName2);   //Two Sailors for the race
                    }
                }
                return;
            }  //if match mode in race continue 
        }else{
            llMessageLinked(LINK_THIS,7200,"sendmsg",ownerDName+"  ID"+strId+" wrong way!");
            llMessageLinked(LINK_THIS,7200,"chanmsg","badir,"+(string)ownerKey+","+strId+","+(string)ways);
            return;
        }
    }
    if(diff<0){ //early. diff is not valid for multistart  chronometer<0
        if(lineMode!=2 && lineMode!=7 && lineMode!=3){  //Trial Series or trial or multistart
            llMessageLinked(LINK_THIS,7200,"sendmsg",ownerDName+"  ID"+strId+" is over early!  Go around and recross!");
            llMessageLinked(LINK_THIS,7200,"chanmsg","early,"+(string)ownerKey+","+strId+","+(string)diff+","+(string)now);        
            return;
        }
    }
    
    if(lineMode==1){ //match mode   here it comes only when the race have started 
        if(sailor1!=(string)ownerKey+strId && sailor2!=(string)ownerKey+strId) return;  //Check that it is one of the two sailors
    }
    string s=ownerDName+","+ownerName+","+(string)ownerKey+","+(string)diff+","+(string)now+","+strId+","+strType;
    llMessageLinked(LINK_THIS, 2000, "crossing", s);
    
    return;
}

handle_crossing(string list_argument) {
    //list parse=llParseString2List(list_argument, ["|"], []);
    list parse=llCSV2List(list_argument);
    string ownerDName=llList2String(parse,0);   //boat owner display name
    string ownerName=llList2String(parse,1);    //boat owner name
    key ownerKey=llList2Key(parse,2);           //boat owner key
    integer raceTimeRel=llList2Integer(parse,3);
    integer raceTimeAbs=llList2Integer(parse,4);
    string raceID=llList2String(parse,5);
    string boatType=llList2String(parse,6);
    integer listIdx=llListFindList(ownerList,[(string)ownerKey+raceID]);
    integer lapTime;
    string msg;
    integer lapNum;

    //llOwnerSay("ownerName="+ownerName);
    //llOwnerSay("ownerkey="+(string)ownerKey);
    //llOwnerSay("raceID="+raceID);
    //llOwnerSay("raceTimeRel="+(string)raceTimeRel);
    if(raceTimeRel<0){
        if(lineMode==2 || lineMode==7){  //Trial Series or trial
            if(listIdx>=0){   //find it, he has finished his lap when the race is over, it is accepted 
                if(llList2Integer(timeList,listIdx)>0){  //if the time is negative, cross back one lap and continue
                    if(dbg!=0) dbgSay("Racer "+ownerName+" is already finished");
                    return;  
                }
            }else return;  //can't find it  cross out of time
        }
    }
    
    if(listIdx==-1){ //not in list... will add to list if not over early
        if(dbg!=0) dbgSay("Didn't find racer "+ownerName+" in list. Adding him/her");
        if(lineMode==3){  //multistart
            if(startN==1) raceTimeRel=startat1;
            else if(startN==2) raceTimeRel=startat2;
            else if(startN==3) raceTimeRel=startat3;
            else raceTimeRel=startat4;
            raceTimeRel=raceTimeAbs-raceTimeRel;
            if(raceTimeRel<0){
                llMessageLinked(LINK_THIS,7200,"sendmsg",ownerDName+"  ID"+raceID+" is over early!  Go around and recross!");
                llMessageLinked(LINK_THIS,7200,"chanmsg","early,"+(string)ownerKey+","+raceID+","+(string)raceTimeRel+","+(string)raceTimeAbs);        
                return;
            }                
        } else if(lineMode==6) {   //network
            if(nettype!="S") return;
        }
        if(llGetFreeMemory()>2000){   //Reserve 2kb of memory so it doesn't crash
            addRacer(ownerName, ownerKey, raceTimeRel, raceTimeAbs, boatType, raceID);
            if(isCallsValue(1,1,0)) llMessageLinked(LINK_THIS,-22011,(string)raceTimeRel+","+raceID+","+(string)startN,ownerKey);    //call  
            if(isCallsValue(1,1,1)){ 
                msg=ownerDName+" ID"+raceID;
                if(lineMode==2) msg+=" starts a new lap";
                else if(lineMode==7) msg+=" start";
                else if(lineMode==3) msg="Start"+(string)startN+" "+msg+" starts at time "+sec2hhmmss(raceTimeRel);
                else msg+=" starts at time "+sec2hhmmss(raceTimeRel);
                llMessageLinked(LINK_THIS,7200,"sendmsg2",msg); 
            }
            if(lineMode==2 || lineMode==7 || lineMode==4) llMessageLinked(LINK_THIS, 3000, "start", (string)raceTimeAbs);
            if(lineMode!=5){
                llMessageLinked(LINK_THIS,7200,"chanmsg","avsta,"+(string)ownerKey+","+raceID+","+(string)lineMode+","+(string)raceTimeRel+","+(string)raceTimeAbs);
            }else{
                llMessageLinked(LINK_THIS,7200,"chanmsg","avend,"+(string)ownerKey+","+raceID+",0,0,"+(string)raceTimeRel+","+(string)raceTimeAbs);     
            }
        }else{
            llMessageLinked(LINK_THIS,7211,"sendmsg2","");  //No more Racers are allowed due to lack of memory
        }
        return;
    } 
        
    if(dbg!=0) dbgSay("Found racer "+ownerName+" in list. Checking for lap time.");
    if(lineMode==0 || lineMode==1 || lineMode==6){ //in the list, so they must have started already  race, match, network mode
        if (lineMode==6) {
            if(nettype!="F") return;
        }
        
        lapNum=llList2Integer(lapList,listIdx);
        ++lapNum;
        if (lapNum>raceNumLaps){
            if(dbg!=0) dbgSay("Racer "+ownerName+" is already finished");
            return;
        }

        if (llFabs(llList2Float(timeList,listIdx)-raceTimeRel)<=minTime) { //is time dif > min lap time?
            msg="Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.";
            llMessageLinked(LINK_THIS,7200,"sendmsg","/me : "+msg);        
            if(dbg!=0) dbgSay("Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.");
            return;        
        }
        
        lapList=llListReplaceList(lapList,[lapNum],listIdx,listIdx); //update lap number for this racer
        lapTime=llList2Integer(timeList,listIdx);
        lapTime=raceTimeRel-lapTime;
        timeList=llListReplaceList(timeList,[raceTimeRel],listIdx,listIdx); //update race time for this racer
        //add 
        //allLapsList+=["r"+(string)listIdx,lapNum,lapTime];
        llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, (string)lapNum+(string)lapTime);
        if (lapNum==raceNumLaps) {
            if (llListFindList(finishList,[listIdx])==-1){ 
                finishList+=listIdx;
                llMessageLinked(LINK_ALL_OTHERS, 6903, (string)listIdx, "");
            }
            string snumorder=(string)llGetListLength(finishList);
            if(isCallsValue(1,3,0)) llMessageLinked(LINK_THIS,-22013,(string)snumorder+","+(string)lapTime+","+(string)raceTimeRel+","+raceID,ownerKey);    //call 
            if(isCallsValue(1,3,1)){
                if (!winnerFlag) {
                    msg="Winner: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                    winnerFlag=TRUE;
                } else if (!runnerupFlag) {
                    msg="Runner up: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                    runnerupFlag=TRUE;
                } else {
                    msg="#"+snumorder+": "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                }
                llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,(string)raceNumLaps+msg);
                msg="";
            }
            llMessageLinked(LINK_THIS,7200,"chanmsg","avend,"+(string)ownerKey+","+raceID+","+snumorder+","+(string)lapTime+","+(string)raceTimeRel+","+(string)raceTimeAbs);        
        } else if(lineMode==0) {
            if(isCallsValue(1,2,0)) llMessageLinked(LINK_THIS,-22012,(string)lapNum+","+(string)lapTime+","+(string)raceTimeRel+","+raceID,ownerKey);    //call 
            if(isCallsValue(1,2,1)) msg=ownerDName+"  ID"+raceID+": "+(string)lapNum+" lap(s) completed at time "+sec2hhmmss(raceTimeRel)+" -- Laptime is "+sec2hhmmss(lapTime);
            llMessageLinked(LINK_THIS,7200,"chanmsg","avlap,"+(string)ownerKey+","+raceID+","+(string)lapNum+","+(string)lapTime+","+(string)raceTimeRel);   
        }
        if(msg!=""){
            llMessageLinked(LINK_THIS,7200,"sendmsg","/me : "+msg);        
        }
        return;
    } 
    
    if(lineMode==2 || lineMode==7) { //in the list  Trial Series and trail mode
        //if (llFabs(llList2Float(timeList,listIdx)-raceTimeRel)>minTime) { //is time dif > min lap time?
    
        lapTime=llList2Integer(timeList,listIdx);  //list of times 
        if(lapTime<0){ // If time is negative, it is the start time of the lap that is now ending
            lapTime=raceTimeAbs+lapTime; //lap time
            if(lapTime<=minTime){   //is below min lap time
                msg="Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.";
                llMessageLinked(LINK_THIS,7200,"sendmsg","/me : "+msg);        
                if(dbg!=0) dbgSay("Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.");
                return;
            }

            timeList=llListReplaceList(timeList,[lapTime],listIdx,listIdx); //update race time for this lap
            lapNum=llList2Integer(lapList,listIdx); //lap number
            ++lapNum;
            lapList=llListReplaceList(lapList,[lapNum],listIdx,listIdx);  //actual lap number
            llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, (string)lapNum+(string)lapTime);  //update allLapsList
            if(isCallsValue(1,3,0)) llMessageLinked(LINK_THIS,-22013,"0,"+(string)lapTime+","+(string)lapTime+","+raceID,ownerKey);    //call 
            if(isCallsValue(1,3,1)){
                if(lineMode==7){ 
                    msg=ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(lapTime);
                    llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,"1"+msg);
                }else{   //lineMode==2
                    msg=ownerDName+"  ID"+raceID+"!  Lap time: "+sec2hhmmss(lapTime);
                    llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,(string)lapNum+msg);
                }
            }
            llMessageLinked(LINK_THIS,7200,"chanmsg","avend,"+(string)ownerKey+","+raceID+",0,"+(string)lapTime+","+(string)raceTimeRel+","+(string)raceTimeAbs);                 }else if(lineMode==2){  //Trial Series mode start a new lap
            if(llList2Integer(lapList,listIdx)<9){
                timeList=llListReplaceList(timeList,[-raceTimeAbs],listIdx,listIdx); // Update the start time of the lap in negative
                llMessageLinked(LINK_THIS, 3000, "start", (string)raceTimeAbs);
                llMessageLinked(LINK_THIS,7200,"chanmsg","avsta,"+(string)ownerKey+","+raceID+","+(string)lineMode+","+(string)raceTimeRel+","+(string)raceTimeAbs);
                if(isCallsValue(1,1,1)) llMessageLinked(LINK_THIS,7200,"sendmsg",ownerDName+"  ID"+raceID+" starts new lap");
            }else{ 
                msg=ownerDName+"  ID"+raceID+" Only 9 laps are allowed to do";
                llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,"9"+msg);
            }
        }else{ //for trail mode Only one turn is allowed  lineMode==7
            if(dbg!=0) dbgSay("Racer "+ownerName+" is already finished");
            msg=ownerDName+"  ID"+raceID;
            llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,"1"+msg);
        }
        return;
    }
    
    if(lineMode==3){ //in the list, so they must have started already  Multistart
        if(dbg!=0) dbgSay("MultiStart Found racer "+ownerName+" in list. Checking for lap time.");
        
        integer vStartN=(integer)llList2String(startList,listIdx);  //start Number for this racer
        if(vStartN==1) raceTimeRel=startat1;
        else if(vStartN==2) raceTimeRel=startat2;
        else if(vStartN==3) raceTimeRel=startat3;
        else raceTimeRel=startat4;
        raceTimeRel=raceTimeAbs-raceTimeRel;
        
        if (llFabs(llList2Float(timeList,listIdx)-raceTimeRel)>minTime) { //is time dif > min lap time?
            if(dbg!=0) dbgSay("Racer "+ownerName+" is over min lap time, checking for lap numbers");
            lapNum=llList2Integer(lapList,listIdx);
            ++lapNum;
            
            if (lapNum<=raceNumLaps) {
                lapList=llListReplaceList(lapList,[lapNum],listIdx,listIdx); //update lap number for this racer
                lapTime=llList2Integer(timeList,listIdx);
                lapTime=raceTimeRel-lapTime;
                timeList=llListReplaceList(timeList,[raceTimeRel],listIdx,listIdx); //update race time for this racer
                //add 
                //allLapsList+=["r"+(string)listIdx,lapNum,lapTime];
                llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, (string)lapNum+(string)lapTime);
                if (lapNum==raceNumLaps) {
                    string snumorder;
                    finishList+=[raceTimeRel,listIdx];
                    llMessageLinked(LINK_ALL_OTHERS, 6903, (string)vStartN+(string)listIdx, (string)raceTimeRel);
                    if(vStartN==1){ 
                        finishList1+=listIdx;
                        snumorder=(string)llGetListLength(finishList1);
                    }else if(vStartN==2){ 
                        finishList2+=listIdx;
                        snumorder=(string)llGetListLength(finishList2);
                    }else if(vStartN==3){ 
                        finishList3+=listIdx;
                        snumorder=(string)llGetListLength(finishList3);
                    }else{ 
                        finishList4+=listIdx;
                        snumorder=(string)llGetListLength(finishList4);
                    }
                    --vStartN;
                    if(isCallsValue(1,3,0)) llMessageLinked(LINK_THIS,-22013,(string)snumorder+","+(string)lapTime+","+(string)raceTimeRel+","+raceID,ownerKey);    //call 
                    if(isCallsValue(1,3,1)){
                        if(!llList2Integer(lwinnerFlag,vStartN)){
                            msg="Start"+(string)(vStartN+1)+" Winner: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                            lwinnerFlag=llListReplaceList(lwinnerFlag,[TRUE],vStartN,vStartN);
                        }else if(!llList2Integer(lrunnerupFlag,vStartN)) {
                            msg="Start"+(string)(vStartN+1)+" Runner up: "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                            lrunnerupFlag=llListReplaceList(lrunnerupFlag,[TRUE],vStartN,vStartN);
                        } else {
                            msg="Start"+(string)(vStartN+1)+" #"+snumorder+": "+ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(raceTimeRel);
                        }
                        llMessageLinked(LINK_ALL_OTHERS,6910,(string)listIdx,(string)raceNumLaps+msg);
                        msg="";
                    }
                    llMessageLinked(LINK_THIS,7200,"chanmsg","avend,"+(string)ownerKey+","+raceID+","+snumorder+","+(string)lapTime+","+(string)raceTimeRel+","+(string)raceTimeAbs);        
                }
                if(msg!=""){
                    llMessageLinked(LINK_THIS,7200,"sendmsg2",msg);        
                }
            } else {
                if(dbg!=0) dbgSay("Racer "+ownerName+" is already finished");
            }
        } else {
            msg="Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.";
            llMessageLinked(LINK_THIS,7200,"sendmsg2",msg);        
            if(dbg!=0) dbgSay("Racer "+ownerName+" is below min lap time "+(string)minTime+"sec.");
        }
        return;
    } 
    
    if(lineMode==4) { //in the list  race test
        lapNum=llList2Integer(lapList,listIdx);
        if(lapNum==0){ //finish lap
            lapTime=llList2Integer(timeList,listIdx);
            lapTime=raceTimeRel-lapTime;
            timeList=llListReplaceList(timeList,[lapTime],listIdx,listIdx); //update race time for this racer
            lapList=llListReplaceList(lapList,[1],listIdx,listIdx);  //finish lap
            if(isCallsValue(1,3,0)) llMessageLinked(LINK_THIS,-22013,"0,"+(string)lapTime+","+(string)lapTime+","+raceID,ownerKey);    //call 
            if(isCallsValue(1,3,1)){
                msg=ownerDName+"  ID"+raceID+"!  Race time: "+sec2hhmmss(lapTime);
                llMessageLinked(LINK_THIS,7200,"sendmsg","/me : "+msg);        
            }
            llMessageLinked(LINK_THIS,7200,"chanmsg","avend,"+(string)ownerKey+","+raceID+",0,"+(string)lapTime+","+(string)raceTimeRel+","+(string)raceTimeAbs);        
        }else{  //start again
            llMessageLinked(LINK_THIS, 3000, "start", (string)raceTimeAbs);
            llMessageLinked(LINK_THIS,7200,"chanmsg","avsta,"+(string)ownerKey+","+raceID+","+(string)lineMode+","+(string)raceTimeRel+","+(string)raceTimeAbs);
            lapList=llListReplaceList(lapList,[0],listIdx,listIdx);  //start 
            timeList=llListReplaceList(timeList,[raceTimeRel],listIdx,listIdx); //update start time for this racer
            if(isCallsValue(1,1,1)) llMessageLinked(LINK_THIS,7200,"sendmsg",ownerDName+"  ID"+raceID+" starts at time "+sec2hhmmss(raceTimeRel)); 
        }
    }
}

//<-Detector Block

//Register Block->
//when racer crosses line for the first time (after the start), add place to lists
addRacer(string ownername, key owner, integer startTimeRel, integer startTimeAbs, string boatType, string raceID) {
    integer listIdx=llGetListLength(ownerList);
    lapList+=[0];
    if(lineMode==2 || lineMode==7){  //Trial Series
        timeList+=[-startTimeAbs];
        //allLapsList+=["r"+(string)listIdx,0,startTimeRel];
        llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, "00");        
    }else if(lineMode==3){  //multistart
        startList+=startN;  //last start number
        llMessageLinked(LINK_ALL_OTHERS, 6904, (string)startN, "");   //startlist    
        timeList+=[startTimeRel];
        //allLapsList+=["r"+(string)listIdx,0,startTimeRel];
        llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, "0"+(string)startTimeRel);    
    }else{ 
        timeList+=[startTimeRel];
        //allLapsList+=["r"+(string)listIdx,0,startTimeRel];
        if(lineMode==0 || lineMode==1) llMessageLinked(LINK_ALL_OTHERS, 6902, (string)listIdx, "0"+(string)startTimeRel);    
        if(lineMode==6){
            if(nNetSend==-1) llMessageLinked(LINK_ALL_OTHERS,6000,"netsend2",(string)owner+raceID+"#"+(string)startTimeRel+"#"+ownername+" "+raceID);
        }
    }
    ownerList+=[(string)owner+raceID];
    nameList+=ownername+" ID"+raceID;
    llMessageLinked(LINK_ALL_OTHERS, 6901, ownername+" ID"+raceID, "");
}

dbgSay(string text)
{
    if(dbg!=0){ 
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
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
    nNetSend=0;
    netIndex=0;
    netIndex2=0;
}

Start(integer argument)
{
    start_at=argument;
    //Register reset
    lapList=[];
    timeList=[];
    ownerList=[];
    finishList=[];
    finishList1=[];
    finishList2=[];
    finishList3=[];
    finishList4=[];
    nameList=[];
    startList=[];
    //allLapsList=[]; 
    llMessageLinked(LINK_ALL_OTHERS, 6909, "", "");    //laps ini  
    winnerFlag=FALSE;
    lwinnerFlag=[FALSE,FALSE,FALSE,FALSE];
    runnerupFlag=FALSE;
    lrunnerupFlag=[FALSE,FALSE,FALSE,FALSE];
    mute0=[]; 
    mute1=[];
    mute2=[];
    mute3=[];
    muteIndex=0;    
    //end Register
    swRun=1;
    sailor1="";
    sailor2="";
    matchName1="";
    matchName2="";    
    nNetSend=0;
    netIndex=0;
    netIndex2=0;
    llSetTimerEvent(1.0);
}
/*
sayResults(integer swSend,string results)
{
    if (swSend==0) llMessageLinked(LINK_THIS,7200,"sendmsg","/me :"+results);   
    else llMessageLinked(LINK_ALL_OTHERS, 6000, "results", results); 
}
*/
printArrays()
{
    if(dbg!=0){
        llSleep(0.5);
        dbgSay("\n\nEngine Script");
        dbgSay("lapList="+llDumpList2String(lapList, "|"));
        dbgSay("timeList="+llDumpList2String(timeList, "|"));
        dbgSay("ownerList="+llDumpList2String(ownerList, "|"));
        dbgSay("nameList="+llDumpList2String(nameList, "|"));
        //dbgSay("allLapsList="+llDumpList2String(allLapsList, "|"));
        dbgSay("finishList="+llDumpList2String(finishList, "|"));
        if(lineMode==3){
            dbgSay("startList="+llDumpList2String(startList, "|"));
            dbgSay("finishList1="+llDumpList2String(finishList1, "|"));
            dbgSay("finishList2="+llDumpList2String(finishList2, "|"));
            dbgSay("finishList3="+llDumpList2String(finishList3, "|"));
            dbgSay("finishList4="+llDumpList2String(finishList4, "|"));
            dbgSay("startat="+(string)start_at);
            dbgSay("startat1="+(string)startat1);
            dbgSay("startat2="+(string)startat2);
            dbgSay("startat3="+(string)startat3);
            dbgSay("startat4="+(string)startat4);
        }
        dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    }
}

inicio()
{
        if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
} 

putCall(integer num, string code, string data)  //num=identifier
{
    //llOwnerSay((string)calls);
    integer hexpos=num%10;  //0-7
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
    //llOwnerSay((string)calls);
}

//evaluate a bit of plugin calls list
integer isCallsValue(integer item, integer num, integer bit)  //item=item array, num=Hex pos 0-7, bit=bit 0,1,2  //return 0 or >0 
{
    return llList2Integer(calls,item) & (integer)("0x"+llInsertString("0000000",7-num,llGetSubString("124",bit,bit)));
}

putAction(integer num, string code, string data)
{
}

default 
{
    state_entry()
    {
        integer n;
        start_at=-1;
        swRun=0;
        llSetTimerEvent(0.0);
        for(n=1;n<5;n++) calls+=1717986918;  //0x66666666  list 5 integers 40 values
    }
    
    collision_start(integer num) {
        if(model==0){
            if(dbg!=0) dbgSay("Collision detected");
            if (start_at==-1) {
                if(dbg!=0) dbgSay("I'm not on. leaving premature");
                return;
            }
    
            integer now=llGetUnixTime();    // Get the time as early as possible
            integer diff;
            if(lineMode==2 || lineMode==7) diff=start_at-now;
            else diff=now-start_at;
            if(dbg!=0) dbgSay("Collision detected with "+(string)num+" objects at "+(string)diff);
            integer i;
            for (i=0;i<num;++i) {
                handle_collision(i, diff, now);
            } // for
        }
    }
    
    link_message(integer sender_num, integer num, string str, key id)
    {
        //llOwnerSay("Startline engine: "+(string)num+" "+str+" "+(string)id);        
        if(num>=22000 && num<23000){ 
            if(llGetSubString(str,0,0)=="2") putAction(num-22000,str,(string)id); //plugin calls action
            else putCall(num-22000,str,(string)id);  //plugin calls code
            return;
        }
        if (num>=0 && num<1000) {
            if (str=="start") {
                if(lineMode==3){
                    integer n=llSubStringIndex((string)id,",");
                    startN=num;
                    start_at=(integer)((string)id);
                    if(startN==1){ 
                        startat2=startat3=startat4=0;
                        startat1=start_at;
                        Start(start_at);
                    }else if(startN==2) startat2=start_at;
                    else if(startN==3) startat3=start_at;
                    else startat4=start_at;
                } else Start((integer)((string)id));
                resultsMode=lineMode;
                return;
            }
            if (str=="restart") {
                Restart();
                return;
            }
            if (str=="modedata") {
                if(lineMode==1){
                    list li=llCSV2List(id);
                    time1=(integer)llList2String(li,0);
                    time2=(integer)llList2String(li,1);
                    time3=(integer)llList2String(li,2);
                }
                return;
            }
            if (str=="setopt") {
                list parse=llCSV2List(id);
                //setDName=(integer)llList2String(parse,0);
                minTime=(integer)llList2String(parse,1);
                ways=(integer)llList2String(parse,2);
                raceNumLaps=(integer)llList2String(parse,3);
                lineMode=(integer)llList2String(parse,5);
                return;
            }
            if (str=="config") {
                list l=llCSV2List(id); 
                channel=(integer)llList2String(l,0);
                pingChannel=(integer)llList2String(l,1);
                setDName=(integer)llList2String(l,2);
                dbg=(integer)llList2String(l,8);
                region_relay_channel=(integer)llList2String(l,9);
                model=(integer)llList2String(l,14);
                inicio();
                return;
            } 
        }

        if(num<2000 || num>2999) return;  //Aux receive only between 2000 and 2999
        
        if (str=="crossing") {
            handle_crossing((string)id);
            return;
        }
        if (str=="results") {
            if (dbg!=0) printArrays();
            if(resultsMode==1) llMessageLinked(LINK_ALL_OTHERS,6801,llList2CSV(lapList),llList2CSV(timeList));  //1-match
            else if(resultsMode==2) llMessageLinked(LINK_ALL_OTHERS,6802,"","");   //Trial Series
            else if(resultsMode==3){    //multistart
                integer startNum=(integer)llGetSubString((string)id,0,0);
                string ptype=llGetSubString((string)id,1,-1);
                if(ptype=="for start"){
                    llMessageLinked(LINK_ALL_OTHERS,6803,"0"+(string)startNum+(string)raceNumLaps+llList2CSV(lapList),llList2CSV(timeList));
                }else if(llGetSubString(ptype,0,5)=="start."){
                    startNum=(integer)llGetSubString(ptype,6,6);
                    llMessageLinked(LINK_ALL_OTHERS,6803,"1"+(string)startNum+(string)raceNumLaps+llList2CSV(lapList),llList2CSV(timeList));
                }else if(ptype=="together"){
                    llMessageLinked(LINK_ALL_OTHERS,6803,"20"+(string)raceNumLaps+llList2CSV(lapList),llList2CSV(timeList));
                }
            //else if(lineMode==3) llMessageLinked(LINK_ALL_OTHERS,6803,(string)startNum+(string)raceNumLaps+llList2CSV(lapList),llList2CSV(timeList));
            }else if(resultsMode==4) llMessageLinked(LINK_ALL_OTHERS,6804,llList2CSV(lapList),llList2CSV(timeList));   //race training
            else if(resultsMode==5) llMessageLinked(LINK_ALL_OTHERS,6805,"",llList2CSV(timeList));  //start training
            else if(resultsMode==6) llMessageLinked(LINK_ALL_OTHERS,6806,llList2CSV(lapList),llList2CSV(timeList));  //6-network
            else if(resultsMode==7) llMessageLinked(LINK_ALL_OTHERS,6807,"","");  //Trial
            else llMessageLinked(LINK_ALL_OTHERS,6800,(string)raceNumLaps+llList2CSV(lapList),llList2CSV(timeList));  //0-race
            return;
        }
        if (lineMode==6){   //network
            if (str=="netsend"){  //from startline script send data to finish line
                integer nn=llGetListLength(ownerList);
                if(nn<=netIndex || nNetSend<0){ 
                    if(nNetSend>=0) nNetSend=-1;
                    llMessageLinked(LINK_THIS,1000,"netsendstop","");   //stop timer send data
                    return;
                }
                if (nn>netIndex+10) nn=netIndex+10;
                nNetSend++;
                //llOwnerSay("startlineM enviando datos "+(string)nNetSend);
                llMessageLinked(LINK_ALL_OTHERS,6000,"netsend2",llList2CSV(llList2List(ownerList,netIndex,nn-1))+"#"+llList2CSV(llList2List(timeList,netIndex,nn-1))+"#"+llList2CSV(llList2List(nameList,netIndex,nn-1)));
                netIndex=nn;   //sailors start number as send to remote line
            } else if (str=="netrdata"){   //finish line receive data from start line
                if(num==2001){  //reconnect finish line
                    ownerList=[];
                    nameList=[];
                    timeList=[];
                }
                list l=llParseString2List((string)id,["#"],[""]);
                integer vt=llGetListLength(ownerList);
                ownerList+=llCSV2List(llList2String(l,0));   //ownerlist
                string vnames=llList2String(l,2);
                nameList+=llCSV2List(vnames);   //namelist
                
                l=llCSV2List(llList2String(l,1));   //timelist
                timeList+=l;
                integer vn=llGetListLength(l);
                integer i;
                for(i=0;i<vn;i++){
                    lapList+=0;
                    //allLapsList+=["r"+(string)(vt+i),0,(integer)llList2String(l,i)];
                    llMessageLinked(LINK_ALL_OTHERS, 6902, (string)(vt+i), "0"+llList2String(l,i));  //add racer to allLapsList
                }
                llMessageLinked(LINK_ALL_OTHERS,6911,vnames,"");   //send names to results 
                //llOwnerSay("Procesando datos "+llDumpList2String(ownerList,","));
            } else if (str=="nettype") {
                nettype=(string)id;
            } else if (str=="netresend"){  //from sendresults script for resend data for a reconnect
                //llOwnerSay("*** engine netresend "+(string)netIndex2+" "+(string)id);
                if((integer)((string)id)==0) netIndex2=0;
                integer nn=llGetListLength(ownerList);
                nNetSend=-2;
                if(nn>netIndex2){ 
                    if (nn>netIndex2+10) nn=netIndex2+10;
                    else nNetSend=-1;
                    llMessageLinked(LINK_ALL_OTHERS,6000,"netresend2",llList2CSV(llList2List(ownerList,netIndex2,nn-1))+"#"+llList2CSV(llList2List(timeList,netIndex2,nn-1))+"#"+llList2CSV(llList2List(nameList,netIndex2,nn-1)));
                    netIndex2=nn;   //sailors start line number as send to remote line
                }
            }
        
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