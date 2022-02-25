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
//StartLine v2.0Beta
//About this script - StartLine results
//This script is used to send the results to the chat without any name.
//the Engine script sends the elements of the arrays necessary to display the results

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
string name;
integer dbg;
integer resultsMode;

integer model;
integer channel;
integer flyChannel;
string flyNum;
integer region_relay_channel;
integer helloChannel;

string vstr;
integer raceNumLaps;
key netTarget; //**
integer swWaiting;
key netAvi;
string netLineUrl;
string netRemoteUrl;
integer swreconnect;
key urlRequestId;
key http_connect_id;
key http_start_id;
key http_data_id;
key http_redata_id;
key http_restart_id;
key http_sets_id;   //key of llHTTPRequest 

integer LEFTLINE=0;
integer RIGHTLINE=0;
integer start_at=-1;

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

sayResults(integer swSend,string results)
{
    if (swSend<=0) {
        if(swSend<0) results="There are no results to show ";
        if(model==1) llShout(flyChannel-1,flyNum+"semsg"+"|/me : "+results);             
        else llShout(0,"/me : "+results);
        if(region_relay_channel!=0) llRegionSay(region_relay_channel,results);
    } else {
        llSetObjectName(" ");
        if(model==1) llShout(flyChannel-1,flyNum+"resul"+"|/me : "+results);             
        else llShout(0,"/me "+results);
        if(region_relay_channel!=0) llRegionSay(region_relay_channel,results);
        llSetObjectName(name);
        return;
    }
}

integer Results(integer pStart, integer praceNumLaps)
{
    integer len2;
    len2=llGetListLength(lapList);
    if(len2==0){
        sayResults(-1,""); 
        return 0;
    }

    string results="";
    integer sw=0;
    string line;
    key ownerKey;
    integer i;
    integer y;
    integer n=0;
    integer listIdx;
    integer len;
    integer swSend=0;
    list finList;
    if(resultsMode==3){ 
        results="\nMultiStart ";
        if(pStart==9) results+="All Starts Results";
        else{ 
            results+="Start "+(string)pStart+" Results";
            len=llGetListLength(lapList);
            sw=0;
            for (i=0;i<len;++i) {   
                if(llList2Integer(startList,i)==pStart){ 
                    sw=1;
                    i=len;
                }            
            }
            if(sw==0){
                results+="\nThere are no results to show";
                sayResults(swSend,results);
                return 1;
            }
        }
    }else results+="\nRace Results:";
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
            if(resultsMode==3 && pStart>0 && pStart<9){
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
            if(resultsMode==3 && pStart>0 && pStart<9){
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
    if(dbg!=0){
        llSleep(0.5);
        dbgSay("\n\nresults script");
        dbgSay("lapList="+llDumpList2String(lapList, "|"));
        dbgSay("timeList="+llDumpList2String(timeList, "|"));
        //dbgSay("ownerList="+llDumpList2String(ownerList, "|"));
        dbgSay("nameList="+llDumpList2String(nameList, "|"));
        dbgSay("allLapsList="+llDumpList2String(allLapsList, "|"));
        dbgSay("finishList="+llDumpList2String(finishList, "|"));
        if(resultsMode==3){
            dbgSay("startList="+llDumpList2String(startList, "|"));
            dbgSay("finishList1="+llDumpList2String(finishList1, "|"));
            dbgSay("finishList2="+llDumpList2String(finishList2, "|"));
            dbgSay("finishList3="+llDumpList2String(finishList3, "|"));
            dbgSay("finishList4="+llDumpList2String(finishList4, "|"));
        }
        dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    }
}

Results2()  //Trial Series
{
    integer len;
    len=llGetListLength(timeList);  //timelist is the list of the best times of each racer
    if(len==0){
        sayResults(-1,"");  //there is no data
        return;
    }
    
    integer i;
    integer n;
    list ltime;
    string line;
    string results="\nTrial Series Results:";
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
        sayResults(-1,"");  //there is no data
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
        sayResults(-1,"");  //there is no data
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
        sayResults(-1,""); 
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
        if(num>=0 && num<1000){
            if(str=="start"){ 
                resultsMode=lineMode;
                start_at=(integer)((string)data);
                return;
            }
            if(str=="restart"){ 
                start_at=-1;
                return;
            }
            if(str=="mode"){
                if(num==6){ //change to network mode
                    if(lineMode!=6){
                        swreconnect=0;
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
                helloChannel=(integer)llList2String(l,17);
                if(dbg!=0) dbgSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
                //region_relay_channel=num;
                return;
            }
            return;   
        }
        if(num<6000 || num>6999) return;

        if(num>=6900){ //arrays controls
            if(num==6901){
                nameList+=str;    //ownername+" ID"+raceID;
            }else if(num==6902){ //update allLapsList with last lap
                num=(integer)str;
                if(lineMode==2 || lineMode==7){  //Trial Series and trail
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
                if(lineMode==0 || lineMode==1 || lineMode==3 || lineMode==6){   //race, match, multistart
                    for (y=0;y<=n2;++y){ 
                        idx=llListFindList(allLapsList,["r"+(string)num,y]);
                        if (idx>=0){
                            if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                            else if(y==n2)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                            else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                        }
                    }
                }else if(lineMode==2 || lineMode==7){   //trial series and trial
                    integer laptime;
                    integer n3;
                    integer n4;
                    integer n5;
                    string vName=llList2String(nameList,num);
                    integer idx0;
                    integer idx1;
                    if(n2>0){   //laps number
                        laptime=llList2Integer(timeList,num);  //best personal time
                        if(lineMode==2){ //trial series display laps
                            for (y=1;y<=n2;++y){ 
                                idx=llListFindList(allLapsList,["r"+(string)num,y]);  
                                if (idx>=0){
                                    s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                                }
                            }
                            //s+="\nYour best time: "+sec2hhmmss(laptime);
                            s+="\n"+vName+" best time: "+sec2hhmmss(laptime);
                        }else{
                            //s="\nYour time: "+sec2hhmmss(laptime);
                            s="\n"+vName+" time: "+sec2hhmmss(laptime);
                        }
                        n2=llGetListLength(timeList);  //number of racers
                        n4=0;
                        n5=999999;
                        n1=0;
                        //Look for the position of Racer, the previous and the later
                        for(y=0;y<n2;y++){
                            if(y!=num){
                                n3=llList2Integer(timeList,y);   //tiempo de la lista
                                if(n3<=laptime) ++n1; //si tiempo de la lista menor o igual que tiempo vuelta suma uno a la posicion
                                if(n3<=laptime && n3>=n4){ //si tiempolista <= tiempovuelta y tiempolista >= que tiempoinferior guarda inmediato inferior
                                    n4=n3; 
                                    idx0=y;
                                }if(n3>laptime && n3<=n5){ //si tiempolista >= tiempovuelta y tiempolista <= que tiemposuperior guarda inmediato superior
                                    n5=n3; 
                                    idx1=y;
                                }
                            }
                        }
                        ++n1;
                        s+="\n";
                        //if(n4>0) s+="Previous #"+(string)(n1-1)+"="+sec2hhmmss(n4)+"  ";
                        //s+="You #"+(string)(n1)+"="+sec2hhmmss(laptime)+"  ";
                        //if(n5<999999) s+="Later #"+(string)(n1+1)+"="+sec2hhmmss(n5)+"  ";
                        if(n4>0) s+="#"+(string)(n1-1)+" "+llList2String(nameList,idx0)+" = "+sec2hhmmss(n4)+"  ";
                        s+="#"+(string)(n1)+" *"+vName+"* = "+sec2hhmmss(laptime)+"  ";
                        if(n5<999999) s+="#"+(string)(n1+1)+" "+llList2String(nameList,idx1)+" = "+sec2hhmmss(n5)+"  ";
                    }
                }
                s=llGetSubString(data,1,-1)+s;
                if(model==1) llShout(flyChannel-1,flyNum+"semsg"+"|/me : "+s);  
                else llShout(0,"/me : "+s); 
                if(region_relay_channel!=0) llRegionSay(region_relay_channel, s);
            }else if(num==6911){
                nameList+=llCSV2List(str);    //Network Mode ownername+" "+raceID  Remote line receive names from engine
            }
            return;
        }
        if(num<6800){ //other thinks
            if(str=="name"){
                name=data;
                llSetObjectName(name);
                return;
            }
            if (str=="waiting") {  //network waiting mode
                if(num==6011){
                    if(netLineUrl!=""){ 
                        swWaiting=11;
                        llShout(helloChannel,"netStartAns,1,"+llGetRegionName()+","+netLineUrl+","+(string)llGetKey());
                        netTarget=NULL_KEY;
                    }else llShout(helloChannel,"netError,3");  //3-no url was found
                }else if(num==6012){
                    if(netLineUrl!=""){ 
                        swWaiting=12;
                        llShout(helloChannel,"netFinishAns,1,"+llGetRegionName()+","+netLineUrl+","+(string)llGetKey());
                        netTarget=NULL_KEY;
                    }else llShout(helloChannel,"netError,3");  //3-no url was found
                }else if(swWaiting){  //stop waiting
                    swWaiting=0;
                    llSetTimerEvent(0.0);
                    if(data) llMessageLinked(LINK_ALL_OTHERS, 7100, "stopwait", data);
                }
                return;
            } 
            if(str=="flynum"){
                flyNum=data;
                return;
            }        
            if (str=="netsend2") {  //from engine script to remote line netRData
                http_data_id = llHTTPRequest(netRemoteUrl+"/?netRData", [HTTP_METHOD,"POST"], (string)data);
                return;
            }
            if (str=="netremotestart") {  //send start from start line to finish line
                http_start_id = llHTTPRequest(netRemoteUrl+"/?netRStart", [HTTP_METHOD,"POST"], (string)data);
                return;
            } 
            if (str=="netrestart") {   //send restart origen line
                http_restart_id = llHTTPRequest(netRemoteUrl+"/?netRestart", [HTTP_METHOD,"POST"], (string)data);
                return;
            } 
            if(str=="netconnect"){  //send to finish connect email
                //data=netlinkKey,id,llGetRegionName()
                llMessageLinked(LINK_ALL_OTHERS,7005,llGetSubString((string)data,37,-1)+","+netLineUrl,(key)llGetSubString((string)data,0,35));
                netAvi=(key)llGetSubString(data,37,72);
                llRegionSayTo(netAvi,0,"Waiting for response from remote line");
                llSetTimerEvent(30.0);
                return;
            }
            if (str=="nettime") {   //send to finish, countdown time
                llHTTPRequest(netRemoteUrl+"/?netTime", [HTTP_METHOD,"POST"], (string)data);
                return;
            } 
            if (str=="netdescon") {   //send to remote, desconnect message
                llMessageLinked(LINK_ALL_OTHERS,7100,"netdescon",netAvi);
                llHTTPRequest(netRemoteUrl+"/?netDescon", [HTTP_METHOD,"POST"], "");
                netAvi==NULL_KEY;
                netRemoteUrl="";
                return;
            } 
            if (str=="netreconnect") {   //finish line reconnect
                swreconnect=1;
                urlRequestId=llRequestURL();   //requests a url
                return;
            } 
            if (str=="netresend2") {  //from start line engine script to finish line send data netRData
                http_redata_id = llHTTPRequest(netRemoteUrl+"/?netReData", [HTTP_METHOD,"POST"], (string)data);
                return;
            }
            if (str=="netFresendnew") {  //from finish line engine script to start line request new data
                http_redata_id = llHTTPRequest(netRemoteUrl+"/?netSReNewData", [HTTP_METHOD,"POST"], "");
                return;
            }
            if(str=="netinvert"){  //send to remote line to invert type
                http_sets_id = llHTTPRequest(netRemoteUrl+"/?netinvert", [HTTP_METHOD,"POST"], (string)data);
                return;
            }
            if(str=="netinvertres"){  //response in remote line of netinvert llhttprequest 
                llHTTPResponse((key)llGetSubString(data,45,-1),200,llGetSubString(data,0,44));  //nettype+avi id
                return;
            }
            if(str=="netHudRestore2"){  //from startline script, send response to hud 
                string stype=llGetSubString(data,0,0);
                if(stype=="T") swWaiting=11;
                else if(stype=="G") swWaiting=12;
                netAvi="";
                netRemoteUrl="";
                llHTTPResponse((key)llGetSubString(data,2,-1),200,"OK");
                return;
            }
            
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
            }else if(num==6802){   //Trial Series
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
            if (method == URL_REQUEST_GRANTED) { 
                netLineUrl = body;
                if (swreconnect==1) {
                    http_connect_id = llHTTPRequest(netRemoteUrl+"/?netReconnect", [HTTP_METHOD,"POST"], netLineUrl+"+"+llGetRegionName());
                }
            }else if(method==URL_REQUEST_DENIED) llShout(0,"URL request denied");
        }else{
            if (method == "POST"){
                string clave=llGetHTTPHeader(id,"x-query-string");
                if(clave=="netRStart") {   //network finish line receive message start
                    llMessageLinked(LINK_ALL_OTHERS, 1000, "netrstart", body);   //to startline script body=start_at
                    if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7000, "CountDown Start", netAvi);   //instant message 
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netRData") {   //network remote line receive data from start line
                    llMessageLinked(LINK_ALL_OTHERS,2000,"netrdata",body);   //to engine data from start line
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netRestart") {   //network line receive restart
                    llMessageLinked(LINK_ALL_OTHERS,1000,"netrrestart",body);   //to startline script restart line
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netTime") {   //network line receive time
                    llMessageLinked(LINK_ALL_OTHERS, 1999, body, netAvi);
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netDescon") {   //network line desconnect
                    llMessageLinked(LINK_ALL_OTHERS, 1998, "", netAvi);
                    llHTTPResponse(id,200,"OK");
                    llMessageLinked(LINK_ALL_OTHERS,7100,"netdescon",netAvi);
                    netAvi=NULL_KEY;
                    netRemoteUrl="";
                } else if (clave=="netReconnect") { //receive from finish line.  reconnect lines after sim reset
                    list l=llParseString2List(body,["+"],[]);
                    netRemoteUrl=llList2String(l,0);
                    if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7100, "reconnectok", netAvi);   //connect established
                    llHTTPResponse(id,200,"OK");
                    llMessageLinked(LINK_ALL_OTHERS, 2000, "netresend", "0");  //send sailors from start line to finish line
                } else if(clave=="netReData") {   //network finish line receive data from start line for reconnect
                    if(swreconnect==1){
                        allLapsList=[];
                        nameList=[];
                    }
                    llMessageLinked(LINK_ALL_OTHERS,2000+swreconnect,"netrdata",body);   //to engine data from start line  2001 reconnect clear data
                    if(swreconnect==1) swreconnect++;  //2002 only send data
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netSReNewData") {   //network start line request new data reconnect
                    llMessageLinked(LINK_ALL_OTHERS, 2000, "netresend", "1");
                    llHTTPResponse(id,200,"OK");
                } else if(clave=="netinvert") {   //request for type change
                    llMessageLinked(LINK_ALL_OTHERS, 1000, "netinvertex", body+(string)id);   //nettype+avi id+http id
                } else if(clave=="netHudConnect") {   //request connection from hud
                    if(lineMode!=6){ llHTTPResponse(id,200,"KO,The line is not in Network mode"); return;}
                    else if(start_at!=-1){ llHTTPResponse(id,200,"KO,The line is running"); return;}
                    
                    //0-aviID,1-remoteregion,2-remoteurl,3-type,4-remotekey,5-lockTime
                    list l=llCSV2List(body);
                    if(swWaiting!=11 && swWaiting!=12){ 
                        llHTTPResponse(id,200,"KO,The line is not in a state waiting");
                    }else if(!(swWaiting==11 && llList2String(l,3)=="S") && !(swWaiting==12 && llList2String(l,3)=="F")){ 
                        llHTTPResponse(id,200,"KO,The line does not have the right type");
                    }else{
                        netAvi=(key)llList2String(l,0);
                        netRemoteUrl=llList2String(l,2);   //remote url
                        swWaiting=0;
                        //send: type,remoteregion,remotekey,lockTime,netAvi
                        llMessageLinked(LINK_ALL_OTHERS, 1000, "netrtype", llList2String(l,3)+","+llList2String(l,1)+","+llList2String(l,4)+","+llList2String(l,5)+","+(string)netAvi); //to startline script
                        llHTTPResponse(id,200,"OK");
                    }
                } else if(clave=="netHudRestore") {   //Restore wait state from hud
                    if(lineMode!=6) llHTTPResponse(id,200,"KO,The line is not in Network mode");
                    else if(start_at>=0) llHTTPResponse(id,200,"KO,The line is running");
                    else llMessageLinked(LINK_ALL_OTHERS, 1000, "netHudRestore", body+","+(string)id); //type,owner,request_id
                }else{
                    llHTTPResponse(id,404,"KO");
                }
            } 
        }
    }    
    
    http_response(key request_id, integer status, list metadata, string body) //network mode start line receive response
    {
        if (request_id == http_connect_id){ 
            if(status==200) llMessageLinked(LINK_ALL_OTHERS, 5011, "changecol", "<0,1,0>");  //receive remote line paint F to green
        }else if (request_id == http_start_id){  //receive the line send start command
        }else if (request_id == http_data_id){   //receive start line 
        }else if (request_id == http_restart_id){ //receive the line send restart command
        }else if(request_id==http_sets_id){  //netinvert
            if(status==200){
                if(llGetSubString(body,0,7)=="OKnetinv"){
                    llMessageLinked(LINK_ALL_OTHERS, 1001, "netinvertex", llGetSubString(body,8,8));   //nettype
                    llMessageLinked(LINK_ALL_OTHERS, 7100, "netinvertok", (key)llGetSubString(body,9,-1));
                }else llMessageLinked(LINK_ALL_OTHERS, 7100, "netinvertko", (key)llGetSubString(body,9,-1));
            }
        }
    }
/*    
    email(string time, string address, string subject, string message, integer num_left)  //network mode, receive message from finish line
    {
        if(swWaiting){    //this is the line waiting (finish line)
            if(subject=="netConnectReq"){   //message to connect
                string vs=llDeleteSubString(message, 0, llSubStringIndex(message,"\n\n")+1);
                list l=llCSV2List(vs);
                //data=id,llGetRegionName(),netremoteURL
                netAvi=(key)llList2String(l,0);
                if(netAvi) llMessageLinked(LINK_ALL_OTHERS, 7100, "connectrec", netAvi);   //Connection received
                string vr=llList2String(l,1);  //region name
                netRemoteUrl=llList2String(l,2);   //remote url
                http_connect_id = llHTTPRequest(netRemoteUrl+"/?netConnectAns", [HTTP_METHOD,"POST"], netLineUrl+"+"+llGetRegionName());
                swWaiting=0;
                llSetTimerEvent(0.0);
                llMessageLinked(LINK_ALL_OTHERS, 1001, "netrtype", vr);   //line finish mode
            }
        }
    }
*/    
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












