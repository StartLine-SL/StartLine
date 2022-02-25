integer helloChannel=-95456284;
integer dialogChannel;
integer dialogHandle;
integer helloHandle;
key owner;
vector colorApagado=<1,1,1>;
integer lockTime=60;
integer lockState;
integer startState;
string startSim;
string startUrl;
key startKey;
integer finishState;
string finishSim;
string finishUrl;
key finishKey;
key http_start_id;
key http_finish_id;
key http_startR_id;
key http_finishR_id;
key http_startR4_id;
key http_finishR4_id;
key http_startUC_id;
key http_finishUC_id;
integer hudStatus;
integer waitType; //script wait time type

displayMenu()
{
    llListenRemove(dialogHandle);
    dialogHandle = llListen(dialogChannel, "", owner, "");
    string sname="Startline Network Hud\n\nLock Time="+(string)lockTime+" min";
    llDialog(owner, sname, ["Clear","LockTime","Reset","Close"],dialogChannel); 
}

displaySubMenu(string pmenu)
{
    string sname;
    list menu;
    if(pmenu=="LockTime"){
        sname="Lock Time menu. Actual Time="+(string)lockTime+" min";
        menu=["150 min","180 min","240 min","60 min","90 min","120 min","15 min","30 min","45 min","Close","Up"];
    }
    if(sname!=""){
        llListenRemove(dialogHandle);
        dialogHandle = llListen(dialogChannel, "", owner, "");
        llDialog(owner, sname, menu,  dialogChannel); 
    }
}

connect()
{
    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,4,<1,1,1>,1.0]);
    if(!startState) llOwnerSay("The start line is not set");
    else if(!finishState) llOwnerSay("The finish line is not set");
    else{
        integer vlockTime=0;
        if(lockState==1) vlockTime=lockTime*60;
        http_start_id = llHTTPRequest(startUrl+"/?netHudConnect", [HTTP_METHOD,"POST"], (string)owner+","+finishSim+","+finishUrl+",S,"+(string)finishKey+","+(string)vlockTime);
        http_finish_id = llHTTPRequest(finishUrl+"/?netHudConnect", [HTTP_METHOD,"POST"], (string)owner+","+startSim+","+startUrl+",F,"+(string)startKey+","+(string)vlockTime);
    }
}

isConnect(integer ptype, integer pstatus, string msg)
{
    //llOwnerSay((string)ptype+"   "+(string)pstatus+"   "+msg);
    if(pstatus==200){ 
        if(llGetSubString(msg,0,1)=="OK"){      
            if(ptype==0) startState=2;
            else finishState=2;
            hudStatus=2;
            if(startState==2 && finishState==2){
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,4,<0,1,0>,1.0]);
                llOwnerSay("Connection Stablished");
                hudStatus=3;
            }
        }else{
            if(ptype==0) llOwnerSay("Error in the START line: "+llGetSubString(msg,3,-1));
            else llOwnerSay("Error in the FINISH line: "+llGetSubString(msg,3,-1));
            wait(1);
        }
    }else{ 
        if(ptype==0) llOwnerSay("HTTP communication error with the START line");
        else llOwnerSay("HTTP communication error with the FINISH line");
        wait(1);
    }
}

wait(integer ptype)
{
    waitType=ptype;
    llSetTimerEvent(2);
}

restoreLine(integer p)
{
    if(p==1){
        if(startState==2) http_startR4_id = llHTTPRequest(startUrl+"/?netHudRestore", [HTTP_METHOD,"POST"], "T,"+(string)owner);
        if(finishState==2) http_finishR4_id = llHTTPRequest(finishUrl+"/?netHudRestore", [HTTP_METHOD,"POST"], "G,"+(string)owner);
    }else{
        if(startState>0) http_startR_id = llHTTPRequest(startUrl+"/?netHudRestore", [HTTP_METHOD,"POST"], "T,"+(string)owner);
        if(finishState>0) http_finishR_id = llHTTPRequest(finishUrl+"/?netHudRestore", [HTTP_METHOD,"POST"], "G,"+(string)owner);
    }
}

isRestore(integer ptype, integer pstatus, string msg)  //restore the line 
{
    if(pstatus==200){
        if(llGetSubString(msg,0,1)=="OK"){      
            if(ptype==0){ 
                startState=1;
                llOwnerSay("The START line has been restored to the WS state");
            }else{ 
                finishState=1;
                llOwnerSay("The FINISH line has been restored to the WS state");
            }
            llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,4,<1,1,1>,1.0]);
            hudStatus=1;
        }else{
            if(ptype==0) llOwnerSay("START line: "+llGetSubString(msg,3,-1));
            else llOwnerSay("FINISH line: "+llGetSubString(msg,3,-1));
        }
    }else{ 
        if(ptype==0) llOwnerSay("HTTP communication error with the START line");
        else llOwnerSay("HTTP communication error with the FINISH line");
    }
}

isRestore4(integer ptype, integer pstatus, string msg) //restore rest of lines for connect error
{
    if(pstatus==200){
        if(llGetSubString(msg,0,1)=="OK"){      
            if(ptype==0){ 
                startState=1;
                llOwnerSay("The START line has been restored to the WS state");
            }else{ 
                finishState=1;
                llOwnerSay("The FINISH line has been restored to the WS state");
            }
            llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,4,<1,0,0>,1.0]);
            hudStatus=4;
        }else{
            if(ptype==0) llOwnerSay("START line: "+llGetSubString(msg,3,-1));
            else llOwnerSay("FINISH line: "+llGetSubString(msg,3,-1));
        }
    }else{ 
        if(ptype==0) llOwnerSay("HTTP communication error with the START line");
        else llOwnerSay("HTTP communication error with the FINISH line");
    }
}

clearHud()
{
    startState=0;
    finishState=0;
    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,<1,1,1>,1.0,PRIM_COLOR,2,<1,1,1>,1.0,PRIM_COLOR,4,<1,1,1>,1.0]);
    hudStatus=0;
}

default
{
    state_entry()
    {
        owner=llGetOwner();
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,colorApagado,1.0,PRIM_COLOR,2,colorApagado,1.0,PRIM_COLOR,3,colorApagado,1.0,PRIM_COLOR,4,colorApagado,1.0]);
        dialogChannel = -1 - (integer)("0x" + llGetSubString( (string)llGetKey(), -7, -1) );
        llListenRemove(helloHandle);
        helloHandle = llListen(helloChannel, "", "", "");
        hudStatus=0;
    }
    
    on_rez( integer start_param) 
    {
        owner=llGetOwner();
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,colorApagado,1.0,PRIM_COLOR,2,colorApagado,1.0,PRIM_COLOR,3,colorApagado,1.0,PRIM_COLOR,4,colorApagado,1.0]);
        hudStatus=0;
    }    

    touch_start(integer total_number)
    {
        integer face=llDetectedTouchFace(0);
        integer sw=0;
        if(face>0){
            if(face==1) llShout(helloChannel,"netStartReq");    //start
            else if(face==2) llShout(helloChannel,"netFinishReq");   //finish
            else if(face==3){    //lock
                lockState=!lockState;
                if(lockState){ 
                    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,face,<0,1,0>,1.0]);
                    llOwnerSay("The lines will be locked for "+(string)lockTime+" minutes");
                }else llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,face,<1,1,1>,1.0]);
            }else if(face==4){   //connect
                if(hudStatus==3) restoreLine(0);
                else connect();
            }else if(face==5) displayMenu();   //menu
        }
    }
    
    listen(integer chan, string name, key id, string msg)    
    {
        if(chan==helloChannel){
            list l=llCSV2List(msg);
            string cmd=llList2String(l,0);
            if(cmd=="netStartAns"){
                if(llList2String(l,1)=="1"){
                    hudStatus=1; 
                    startState=1;
                    startSim=llList2String(l,2);
                    startUrl=llList2String(l,3);
                    startKey=(key)llList2String(l,4);
                    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,<0,1,0>,1.0]);
                }else if(llList2String(l,1)=="0"){
                    if(finishState==0) hudStatus=0;
                    startState=0;
                    startSim="";
                    startUrl="";
                    startKey="";
                    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,1,<1,1,1>,1.0]);
                }
            }else if(cmd=="netFinishAns"){
                if(llList2String(l,1)=="1"){
                    hudStatus=1;
                    finishState=1;
                    finishSim=llList2String(l,2);
                    finishUrl=llList2String(l,3);
                    finishKey=(key)llList2String(l,4);
                    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,2,<0,1,0>,1.0]);
                }else if(llList2String(l,1)=="0"){
                    if(startState==0) hudStatus=0;
                    finishState=0;
                    finishSim="";
                    finishUrl="";
                    finishKey="";
                    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,2,<1,1,1>,1.0]);
                }
            }else if(cmd=="netError"){
                string st="";
                integer num=(integer)llList2String(l,1);
                if(num==1) st="The Line is waiting";
                else if(num==2) st="The Line is running";
                else if(num==3) st="No url was found";
                else if(num==4) st="The Line is locked";
                if(st!="") llOwnerSay("Error, "+st);  
            }
        }else if(chan==dialogChannel){
            if(msg=="Reset") llResetScript();
            else if(msg=="LockTime") displaySubMenu(msg);
            else if(msg=="Up") displayMenu();
            else if(msg=="Clear") clearHud();
            else if(llGetSubString(msg,-3,-1)=="min") lockTime=(integer)llGetSubString(msg,0,2);    
        }
    }
    
    http_response(key request_id, integer status, list metadata, string body) //network mode startline response
    {
        if (request_id == http_start_id) isConnect(0,status,body); 
        else if (request_id == http_finish_id) isConnect(1,status,body); 
        else if (request_id == http_startR_id) isRestore(0,status,body); 
        else if (request_id == http_finishR_id) isRestore(1,status,body);
        else if (request_id == http_startR4_id) isRestore4(0,status,body); 
        else if (request_id == http_finishR4_id) isRestore4(1,status,body);
    }
    
    timer()
    {
        llSetTimerEvent(0);
        restoreLine(1);
    }
}





