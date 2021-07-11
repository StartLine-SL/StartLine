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
//About this script - StartLine send results
//This script is used to send the results to the chat without any name.



list allLapsList=[];
list timeList=[];
list nameList=[];
list finishList=[];
list finishList1=[];
list finishList2=[];
list finishList3=[];
list finishList4=[];
list startList=[];
list lapList=[];

integer lineMode;
integer region_relay_channel;
string name;
integer dbg;
integer model;
integer channel;
integer flyChannel;
string flyNum;
string vstr;
integer raceNumLaps;
integer swNetCon;
key netTarget; //**
integer swWaiting;
integer swTimer;
key netAvi;
string netLineUrl;
string netRemoteUrl;
key urlRequestId;
key http_connect_id;
key http_start_id;
key http_data_id;
key http_restart_id;

integer LEFTLINE=0;
integer RIGHTLINE=0;

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

string sec2hms(integer seconds) 
{
    string hms;
    if (seconds<0) hms="-";
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

sendMsg(key k, string s)
{
   llRegionSayTo(k,0,s);    
}

sayResults(integer swSend,string results)
{
    if (swSend==0) llMessageLinked(LINK_THIS,6000,"sendmsg2",results);   
    else llMessageLinked(LINK_THIS, 6000, "results", results); 
}

integer Results(integer pStart, integer praceNumLaps)
{
    integer len2;
    len2=llGetListLength(lapList);
    if(len2==0){
        llMessageLinked(LINK_THIS,6010,"sendmsg2",""); 
        return 0;
    }

    string results="";
    if(lineMode==3){ 
        results="\nMultiStart ";
        if(pStart==9) results+="All Starts ";
        else results+="Start "+(string)pStart;
    }
    results+="\nRace Results:";
    string line;
    key ownerKey;
    integer i;
    integer y;
    integer sw=0;
    integer n=0;
    integer listIdx;
    integer len;
    integer swSend=0;
    list finList;
    if(pStart==0) len=llGetListLength(finishList);
    else if(pStart==1) len=llGetListLength(finishList1);
    else if(pStart==2) len=llGetListLength(finishList2);
    else if(pStart==3) len=llGetListLength(finishList3);
    else if(pStart==4) len=llGetListLength(finishList4);
    else if(pStart==9){  //multistart together
        list listOrder=llListSort(finishList,2,1); //sort for time and keep listidx
        len=llGetListLength(listOrder);
        for (i=0;i<len;i+=2) {    //start racers
            finList+=llList2Integer(listOrder,i+1);
        }
        len=llGetListLength(finList);
    }
    for (i=0;i<len;++i) {    //finished racers
        if(pStart==0) listIdx=llList2Integer(finishList,i);
        else if(pStart==1) listIdx=llList2Integer(finishList1,i);
        else if(pStart==2) listIdx=llList2Integer(finishList2,i);
        else if(pStart==3) listIdx=llList2Integer(finishList3,i);
        else if(pStart==4) listIdx=llList2Integer(finishList4,i);
        else if(pStart==9) listIdx=llList2Integer(finList,i);  //multistart together
        line="\n"+(string)(++n)+": ";
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
    for (i=0;i<len2;++i) {   //not finished racers
        sw=1;
        if (llList2Integer(lapList,i)<praceNumLaps) {
            if(lineMode==3 && pStart>0 && pStart<9){
                if(llList2Integer(startList,i)!=pStart) sw=0;            
            }
            if(sw){
                line="\n"+(string)(++n)+": "+llList2String(nameList,i)+" - Not Finished";
                if(llStringLength(results+line)>1000) {
                    sayResults(swSend,results);
                    swSend=1;
                    results=line;
                }else{
                    results+=line;
                }
            }
        }
    }
    sayResults(swSend,results+"\n");

    //lap times
    string s;
    integer idx;
    results="\nLap Times:";
    swSend=1;
    for (i=0;i<len;++i) {    //lap times finished racers
        if(pStart==0) listIdx=llList2Integer(finishList,i);
        else if(pStart==1) listIdx=llList2Integer(finishList1,i);
        else if(pStart==2) listIdx=llList2Integer(finishList2,i);
        else if(pStart==3) listIdx=llList2Integer(finishList3,i);
        else if(pStart==4) listIdx=llList2Integer(finishList4,i);
        else if(pStart==9) listIdx=llList2Integer(finList,i);  //multistart together
        line="\n"+llList2String(nameList,listIdx);
        s="";
        for (y=0;y<=praceNumLaps;++y) {
            idx=llListFindList(allLapsList,["r"+(string)listIdx,y]);
            if (idx>=0) {
                if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                else if(y==praceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
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
    
    for (i=0;i<len2;++i) {   //lap times not finished racers
        sw=1;
        if (llList2Integer(lapList,i)<praceNumLaps) {
            if(lineMode==3 && pStart>0 && pStart<9){
                if(llList2Integer(startList,i)!=pStart) sw=0;            
            }
            if(sw){
                line="\n"+llList2String(nameList,i);
                s="";
                for (y=0;y<=praceNumLaps;++y) {
                    idx=llListFindList(allLapsList,["r"+(string)i,y]);
                    if (idx>=0) {
                        if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                        else if(y==praceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                        else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                    } else {
                        if (y==0) s+=" - Start: Not Finished";
                        else if(y==praceNumLaps)  s+=" - Last lap: Not Finished";
                        else s+=" - Lap "+(string)y+": Not Finished";
                        y=praceNumLaps;
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
    }
    sayResults(swSend,results+"\n");
    return 1;
}

printArrays()
{
    llSleep(0.5);
    dbgSay("\n\nresults script");
    dbgSay("lapList="+llDumpList2String(lapList, "|"));
    dbgSay("timeList="+llDumpList2String(timeList, "|"));
    //dbgSay("ownerList="+llDumpList2String(ownerList, "|"));
    dbgSay("nameList="+llDumpList2String(nameList, "|"));
    dbgSay("allLapsList="+llDumpList2String(allLapsList, "|"));
    dbgSay("finishList="+llDumpList2String(finishList, "|"));
    if(lineMode==3){
        dbgSay("startList="+llDumpList2String(startList, "|"));
        dbgSay("finishList1="+llDumpList2String(finishList1, "|"));
        dbgSay("finishList2="+llDumpList2String(finishList2, "|"));
        dbgSay("finishList3="+llDumpList2String(finishList3, "|"));
        dbgSay("finishList4="+llDumpList2String(finishList4, "|"));
    }

    dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
}

Results2()  //against time
{
    integer len;
    len=llGetListLength(timeList);  //timelist is the list of the best times of each racer
    if(len==0){
        llMessageLinked(LINK_THIS,6010,"sendmsg2","");  //there is no data
        return;
    }
    
    integer i;
    integer n;
    list ltime;
    string line;
    string results="\nAgainst time Results:";
    integer idx;
    integer swSend=0;
    for(i=0;i<len;++i){
        ltime+=[llList2String(timeList,i),i];   //load ltime array with [timelist,idx] for sort times
    }
    ltime=llListSort(ltime,2,TRUE);  //sort times
    for(i=0;i<len;++i){
        idx=llList2Integer(ltime,i*2+1);
        n=llList2Integer(ltime,i*2);
        if(n==999999999){  //has not finished the first lap
            line="\n"+(string)(i+1)+": "+llList2String(nameList,idx)+" - Not Finished";
        }else{
            line="\n"+(string)(i+1)+": "+llList2String(nameList,idx)+" - Fast Lap Time: "+sec2hhmmss(llList2Integer(ltime,i*2));
        }
        if(llStringLength(results+line)>1000) {
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    sayResults(swSend,results+"\n");
    swSend=1;
    //lap times
    results="\nLap Times:";
    integer x;
    list lap;
    string s;
    string lapStr;
    string timeStr;
    for(i=0;i<len;++i){    //racers
        idx=llList2Integer(ltime,i*2+1);
        line="\n"+llList2String(nameList,idx);
        s="";
        x=0;
        n=1;
        while(x>=0){
            x=llListFindList(allLapsList,["r"+(string)idx,n]);
            if (x>=0) {
                s+=" - Lap "+(string)n+": "+sec2hhmmss(llList2Integer(allLapsList,x+2));
                ++n;
            }
        }
        if(s!="") line+=s;
        else line+=" - Not Finished";
        if(llStringLength(results+line)>1000){
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    sayResults(swSend,results+"\n");
}

Results3(integer ptype, integer pstartNum, integer praceNumLaps)  //results multistart
{
    if(ptype==0){  //for starts
        if (Results(1, praceNumLaps)==1){
            Results(2, praceNumLaps);
            if(pstartNum>2) Results(3, praceNumLaps);
            if(pstartNum>3) Results(4, praceNumLaps);
        }
    }else if(ptype==1){   //a start
        Results(pstartNum, praceNumLaps);
    }else if(ptype==2){  //together
        Results(9, praceNumLaps);
    }
}

Results4()  //results race training
{
    integer len=llGetListLength(timeList);  
    if(len==0){
        llMessageLinked(LINK_THIS,6010,"sendmsg2","");  //there is no data
        return;
    }    
    
    string results="\nRace training Results:";
    integer swSend=0;
    string line;
    integer i;
    for (i=0;i<len;++i) {
        line="\n"+llList2String(nameList,i)+" - ";
        if(llList2Integer(lapList,i)==1) line+="Lap Time: "+sec2hhmmss(llList2Integer(timeList,i));
        else line+="Not finished";
        if(llStringLength(results+line)>1000){
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    sayResults(swSend,results+"\n");
}

Results5() //result start training
{
    integer len=llGetListLength(timeList);  
    if(len==0){
        llMessageLinked(LINK_THIS,6010,"sendmsg2","");  //there is no data
        return;
    }    
    
    string results="\nStart training Results:";
    integer swSend=0;
    string line;
    integer i;
    list listOrder=[];
    for (i=0;i<len;++i) {    //start racers
        listOrder+=[llList2Integer(timeList,i),i];
    }
    listOrder=llListSort(listOrder,2,1);
    for (i=0;i<len;++i) {
        line="\n"+(string)(i+1)+": ";
        line+=llList2String(nameList,llList2Integer(listOrder,i*2+1))+" - Start: "+llList2String(listOrder,i*2)+" sec.";
        if(llStringLength(results+line)>1000){
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    sayResults(swSend,results+"\n");
    //if (dbg!=0) printArrays();
}

Results7()
{
    integer len;
    len=llGetListLength(timeList);  //timelist is the list of the best times of each racer
    if(len==0){
        llMessageLinked(LINK_THIS,6010,"sendmsg2",""); 
        return;
    }
    
    integer i;
    list ltime;
    string line;
    integer time;
    string results="\nTrial Results:";
    integer idx;
    integer swSend=0;
    for(i=0;i<len;++i){
        if((integer)llList2String(timeList,i)<0) ltime+=[999999999,i];
        else ltime+=[(integer)llList2String(timeList,i),i];
    }
    ltime=llListSort(ltime,2,TRUE);    //order the times
    for(i=0;i<len;i++){
        idx=llList2Integer(ltime,i*2+1);
        line="\n"+(string)(i+1)+": ";
        time=llList2Integer(ltime,i*2);
        if(time==999999999) line+=llList2String(nameList,idx)+" - Not Finished";
        else line+=llList2String(nameList,idx)+" - Race Time: "+sec2hhmmss(time);
        if(llStringLength(results+line)>1000) {
            sayResults(swSend,results);
            swSend=1;
            results=line;
        }else{
            results+=line;
        }
    }
    sayResults(swSend,results+"\n");
}

default 
{
    state_entry()
    {
    }
    
    changed(integer change)
    {    
        if (change & (CHANGED_REGION_START)){
            if(lineMode==6){
            }
        }
    }
    
    link_message(integer sender_num,integer num,string str,key data) {
        if(num>=14000){
            llOwnerSay("****************************** 14000 "+(string)num+"    "+str+"  "+(string)data);
            return;
        }
        if(num>=0 && num<1000){
            if(str=="mode"){
                if(num==6){ //change to network mode
                    if(lineMode!=6){
                        urlRequestId=llRequestURL();   //when it enters network mode it requests a url
                        llMessageLinked(LINK_ALL_OTHERS,1000,"linekey",llGetKey());   
                    }
                }else{
                    if(lineMode==6){
                        if(netLineUrl!="") llReleaseURL(netLineUrl);
                    }
                }
                lineMode=num;
                return;
            }
            if(str=="reset"){
                llResetScript();
                return;
            }
            if(str=="config"){   //hay que revisar el envio
                list l=llCSV2List(data); 
                channel=(integer)llList2String(l,0);
                dbg=(integer)llList2String(l,8);
                region_relay_channel=(integer)llList2String(l,9);
                model=(integer)llList2String(l,14);
                flyChannel=(integer)llList2String(l,15);
                if(dbg) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                //region_relay_channel=num;
                return;
            }
            return;   
        }
        if(num<6000 || num>6999) return;

        if(num>=6900){ //arrays controls
            if(num==6901){
                nameList+=str;    //ownername+" "+raceID;
            }else if(num==6902){ //update allLapsList with last lap
                num=(integer)str;
                if(lineMode==2 || lineMode==7){  //against time and trail
                    integer lapTime=(integer)llGetSubString(data,1,-1);
                    if((integer)llGetSubString(data,0,0)==0) timeList+=999999999;  //first start add new element
                    else if(lapTime<llList2Integer(timeList,num)) timeList=llListReplaceList(timeList,[lapTime],num,num);  //save minimun time
                    if((integer)llGetSubString(data,0,0)>0) allLapsList+=["r"+(string)num,(integer)llGetSubString(data,0,0),lapTime]; //save lap time
                }else{
                    allLapsList+=["r"+(string)num,(integer)llGetSubString(data,0,0),(integer)llGetSubString(data,1,-1)];  //save lap time
                }
                return;
            }else if(num==6903){ //finishlist
                if(lineMode==3){ //multistart
                    integer nstart=(integer)llGetSubString(str,0,0);
                    integer nIdx=(integer)llGetSubString(str,1,-1);
                    finishList+=[(integer)((string)data),nIdx];   //time, listidx
                    if(nstart==1) finishList1+=nIdx;
                    else if(nstart==2) finishList2+=nIdx;
                    else if(nstart==3) finishList3+=nIdx;
                    else if(nstart==4) finishList4+=nIdx;
                }else{
                    finishList+=(integer)str;
                }
            }else if(num==6904){ //startlist
                startList+=(integer)str;
            }else if(num==6909){   //lapsini  from engine
                allLapsList=[];
                timeList=[];
                nameList=[];
                finishList=[];
                finishList1=[];
                finishList2=[];
                finishList3=[];
                finishList4=[];
                startList=[];
                lapList=[];            
            }else if(num==6910){   //laps say
                num=(integer)str;
                string s="\nLap times";
                integer y;
                integer idx;
                integer n1;
                integer n2=(integer)llGetSubString(data,0,0);  //racenumlaps or numlap
                if(lineMode==0 || lineMode==1 || lineMode==3){
                    for (y=0;y<=n2;++y){ 
                        idx=llListFindList(allLapsList,["r"+(string)num,y]);
                        if (idx>=0){
                            if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                            else if(y==n2)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                            else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                        }
                    }
                }else if(lineMode==2 || lineMode==7){
                    integer laptime;
                    integer n3;
                    integer n4;
                    integer n5;
                    if(n2>0){   //laps number
                        laptime=llList2Integer(timeList,num);  //best personal time
                        if(lineMode==2){
                            for (y=1;y<=n2;++y){ 
                                idx=llListFindList(allLapsList,["r"+(string)num,y]);  
                                if (idx>=0){
                                    s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                                }
                            }
                            s+="\nYour best time: "+sec2hhmmss(laptime);
                        }else{
                            s="\nYour time: "+sec2hhmmss(laptime);
                        }
                        n2=llGetListLength(timeList);  //racers number
                        n4=0;
                        n5=999999;
                        n1=0;
                        //Look for the position of Racer, the previous and the later
                        for(y=0;y<n2;y++){
                            if(y!=num){
                                n3=llList2Integer(timeList,y);   //tiempo de la lista
                                if(n3<=laptime) ++n1; //si tiempo de la lista menor o igual que tiempo vuelta suma uno a la posicion
                                if(n3<=laptime && n3>=n4) n4=n3; //si tiempolista <= tiempovuelta y tiempolista >= que tiempoinferior guarda inmediato inferior
                                if(n3>laptime && n3<=n5) n5=n3; //si tiempolista >= tiempovuelta y tiempolista <= que tiemposuperior guarda inmediato superior
                            }
                        }
                        ++n1;
                        s+="\n";
                        if(n4>0) s+="Previous #"+(string)(n1-1)+"="+sec2hhmmss(n4)+"  ";
                        s+="You #"+(string)(n1)+"="+sec2hhmmss(laptime)+"  ";
                        if(n5<999999) s+="Later #"+(string)(n1+1)+"="+sec2hhmmss(n5)+"  ";
                    }
                }
                s=llGetSubString(data,1,-1)+s;
                llShout(0,"/me : "+s); 
                if(region_relay_channel!=0) llRegionSay(region_relay_channel, s);
            }            
            return;
        }
        if(num<6800){ //other thinks
            if(str=="sendmsg2"){                //llMessageLinked(LINK_ALL_OTHERS,600N,"sendmsg2","");
                if(num==6000) vstr=(string)data;
                else if(num==6001) vstr="Ready for race.";
                else if(num==6002) vstr="The race is over, timer is stopped.";
                else if(num==6003) vstr="30 seconds to start the countdown.";
                else if(num==6004) vstr="The race start, timer is running.";
                else if(num==6005) vstr="Countdown start, timer is running.";
                else if(num==6006) vstr="Race training start, timer is running.";
                else if(num==6007) vstr="Against time start, timer is running.";
                else if(num==6008) vstr="Start "+(string)data+" is running.";
                else if(num==6009) vstr="Trial start, timer is running.";
                else if(num==6010) vstr="There are no results to show";
                else if(num==6011) vstr="No more Racers are allowed due to lack of memory";
                
                if(model==1) llShout(flyChannel-1,flyNum+"semsg"+"|/me : "+vstr);             
                else llShout(0,"/me : "+vstr);
                if(region_relay_channel!=0) llRegionSay(region_relay_channel,vstr);
                return;
            }                
            if(str=="chanmsg"){
                if(model==1) llShout(flyChannel-1,flyNum+"chmsg"+"|"+(string)(num-6000)+"|"+(string)data);           
                else{
                    if(num==6000) llShout(channel,data);                
                    else llRegionSay(channel,data);   
                }
                return;
            }        
            if(str=="sendmsg"){
                if(model==1) llShout(flyChannel-1,flyNum+"semsg"+"|"+(string)data);             
                else llShout(0,(string)data);
                if(region_relay_channel!=0) llRegionSay(region_relay_channel, data);
                return;
            }   
            if(str=="results"){ //say results
                llSetObjectName(" ");
                if(model==1) llShout(flyChannel-1,flyNum+"semsg"+"|"+(string)data);             
                else llShout(0,"/me "+(string)data);
                if(region_relay_channel!=0) llRegionSay(region_relay_channel, data);
                llSetObjectName(name);
                return;
            }
            if(str=="name"){
                name=data;
                llSetObjectName(name);
                return;
            }        
            if (str=="waiting") {  //network waiting mode remote line
                if(num==6001){
                    swWaiting=1;
                    netTarget=NULL_KEY;
                    llSetTimerEvent(5.0);
                    llGetNextEmail("", "");
                    llMessageLinked(LINK_ALL_OTHERS, 7100, "waiting", data);
                }else{  //stop waiting
                    swWaiting=0;
                    llSetTimerEvent(0.0);
                    llMessageLinked(LINK_ALL_OTHERS, 7100, "stopwait", data);
                }
                return;
            } 
            /*
            if (str=="netcon") {   //main line  starts waiting for the answer 
                swNetCon=6;
                swTimer=0;
                netAvi=(key)llGetSubString(data,0,35);
                netTarget=(key)llGetSubString(data,36,-1);
                llRegionSayTo(netAvi,0,"timer waiting "+(string)swNetCon);
                llSetTimerEvent(5.0);
                llGetNextEmail("", "");
                return;
            }
            */
            if(str=="flynum"){
                flyNum=data;
                return;
            }        
            if(str=="getcode"){
                string sk=(string)llGetKey();
                sk=llGetSubString(sk,0,7)+llGetSubString(sk,9,12)+llGetSubString(sk,14,17)+llGetSubString(sk,19,22)+llGetSubString(sk,24,35);
                llRegionSayTo(data,0,"The code for this line is: "+sk);    
                return;
            }
            if (str=="netsend2") {  //from engine script to remote line netRData
                //llOwnerSay("send results enviando datos");
                http_data_id = llHTTPRequest(netRemoteUrl+"/?netRData", [HTTP_METHOD,"POST"], (string)data);
                //llMessageLinked(LINK_ALL_OTHERS,7008,(string)netTarget,(string)data); 
                return;
            }
            if (str=="netremotestart") {  //send remote start http
                http_start_id = llHTTPRequest(netRemoteUrl+"/?netRStart", [HTTP_METHOD,"POST"], (string)data);
                //llMessageLinked(LINK_ALL_OTHERS,7007,(string)netTarget,(string)data); 
                return;
            } 
            if (str=="netrestart") {   //send restart origen line
                http_restart_id = llHTTPRequest(netRemoteUrl+"/?netRestart", [HTTP_METHOD,"POST"], (string)data);
                //llMessageLinked(LINK_ALL_OTHERS,7009,(string)netTarget,(string)data); 
                return;
            } 
            if(str=="netconnect"){  //send connect email
                //data=netlinkKey,id,llGetRegionName()
                llMessageLinked(LINK_ALL_OTHERS,7005,llGetSubString((string)data,37,-1)+","+netLineUrl,(key)llGetSubString((string)data,0,35));
                netAvi=(key)llGetSubString(data,37,72);
                llRegionSayTo(netAvi,0,"Waiting for response from remote line");
                swTimer=0;
                llSetTimerEvent(30.0);
                return;
            }
            return;
        }
        if(num>=6800){   //results 
            if(num==6800){    //race
                if(llStringLength(str)==1) lapList=[];
                else lapList=llCSV2List(llGetSubString(str,1,-1));
                timeList=llCSV2List(data);
                Results(0, (integer)llGetSubString(str,0,0));
            }else if(num==6801 || num==6806){      //match, network
                if((string)str!="") lapList=llCSV2List(str);
                if((string)data!="") timeList=llCSV2List(data);
                Results(0,1);
            }else if(num==6802){   //against
                Results2(); 
            }else if(num==6803){   //multistart
                if(llStringLength(str)==3) lapList=[];
                else lapList=llCSV2List(llGetSubString(str,3,-1));
                timeList=llCSV2List(data);
                Results3((integer)llGetSubString(str,0,0),(integer)llGetSubString(str,1,1),(integer)llGetSubString(str,2,2));  
            }else if(num==6804){    //race training
                lapList=llCSV2List(str);
                if((string)data!="") timeList=llCSV2List(data);
                Results4();
            }else if(num==6805){   //start training
                if((string)data!="") timeList=llCSV2List(data);
                Results5();
            }else if(num==6807){   //trail
                Results7();
            }
            if (dbg!=0) printArrays();
            return;
        }
    }
    
    http_request(key id, string method, string body)
    {
        if(id==urlRequestId){
            if (method == URL_REQUEST_GRANTED) netLineUrl = body;
            else if(method==URL_REQUEST_DENIED) llShout(0,"URL request denied");
        }else{
            if (method=="GET"){
                list query_args = llParseString2List(llGetHTTPHeader(id,"x-query-string"),["?","=","+","&"],[]);
                if(llList2String(query_args,0)=="asdfadf"){
                }
                llHTTPResponse(id,200,"");                
            }else if (method == "POST"){
                string clave=llGetHTTPHeader(id,"x-query-string");
                //llOwnerSay("http_request "+clave);
                if (clave=="netConnectAns") { //receive start line.  response from remote line connection
                    list l=llParseString2List(body,["+"],[]);
                    netRemoteUrl=llList2String(l,0);
                    if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7100, "connectok", netAvi);   //connect established
                    swNetCon=0;
                    swTimer=1;
                    llSetTimerEvent(0.0);
                    llMessageLinked(LINK_ALL_OTHERS, 1002, "netrtype", llList2String(l,1));  //to startline script
                    llMessageLinked(LINK_ALL_OTHERS, 5011, "changecol", "<0,1,0>");  //to startline script
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netRStart") {   //network remote line receive message start
                    llMessageLinked(LINK_ALL_OTHERS, 1000, "netrstart", body);   //to startline script body=start_at
                    if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7000, "CountDown Start", netAvi);   //instant message 
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netRData") {   //network remote line receive data from start line
                    //llOwnerSay("datos recibidos "+body);
                    llMessageLinked(LINK_ALL_OTHERS,2000,"netrdata",body);   //to engine data from start line
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netRestart") {   //network line receive restart
                    llMessageLinked(LINK_ALL_OTHERS,1000,"netrrestart",body);   //to startline script restart line
                    llHTTPResponse(id,200,"OK");
                }else{
                    llHTTPResponse(id,404,"KO");
                }
            } 
        }
    }    
    
    http_response(key request_id, integer status, list metadata, string body)
    {
        if (request_id == http_connect_id){ 
            if(status==200) llMessageLinked(LINK_ALL_OTHERS, 5011, "changecol", "<0,1,0>");  //receive remote line paint F to green
        }else if (request_id == http_start_id){  //receive the line send start command
        }else if (request_id == http_data_id){   //receive start line 
        }else if (request_id == http_restart_id){ //receive the line send restart command
        }
    }
    
    email(string time, string address, string subject, string message, integer num_left)  //receive message startline link  network mode
    {
        //llOwnerSay("receive email "+address+"   "+message+"   "+(string)num_left);
        if(swWaiting){    //this is the line waiting
            if(subject=="netConnectReq"){   //message to connect
                string vs=llDeleteSubString(message, 0, llSubStringIndex(message,"\n\n")+1);
                list l=llCSV2List(vs);
                //data=id,llGetRegionName(),netremoteURL
                netAvi=(key)llList2String(l,0);
                if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7100, "connectrec", netAvi);   //Connection received
                string vr=llList2String(l,1);  //region name
                netRemoteUrl=llList2String(l,2);   //remote url
                http_connect_id = llHTTPRequest(netRemoteUrl+"/?netConnectAns", [HTTP_METHOD,"POST"], netLineUrl+"+"+llGetRegionName());
                //llMessageLinked(LINK_ALL_OTHERS,7006,(string)netTarget,(string)llGetKey()+","+llGetRegionName());    //send connection answer  startline msg script
                swWaiting=0;
                swTimer=1;  //timer during race
                llSetTimerEvent(0.0);
                llMessageLinked(LINK_ALL_OTHERS, 1001, "netrtype", vr);   //line finish mode
            }
        }
    }
    
    timer()
    {
        if(!swWaiting){
            llSetTimerEvent(0.0);
            llRegionSayTo(netAvi,0,"No response received, connection canceled");
        }else{
            llGetNextEmail("","");
        }
    }
}












