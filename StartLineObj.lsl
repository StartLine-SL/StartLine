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
//About this script - StartLineObj
//This script is used to rez objects with the StartLine Fly model 
//Also controls brightness on Sim models
//
//integer flyChannel=-51234570;
integer flyChannel=-51234580;
integer pingChannel=-2256845;

integer flylistenHandle;
integer flyChannelin;
integer flyObjChannel;
integer flyObjChannelin;
string flyObj;
string flyNum="90000";
vector flyPosition;
integer flyLineRotation;

integer dbg=0;
integer pingHandle;
integer visible=1;
float lineLen;
vector flySimPos;
vector prePos;
integer prePosDir;
rotation preRot;
string simDestName;
integer model;
integer stateline;   //0=stop   1=running

integer currentLine; 
string currentName;
key requestInFlight;

string flyObjPos="C";
vector flyObjOffset=ZERO_VECTOR;
rotation flyObjRot=ZERO_ROTATION;
float flyObjSize=0;
integer flyObjPhantom=1;
integer flyObjAlpha=1;
integer flyObjBright=2;

dbgSay(string text)
{
    if(dbg!=0){  
        if(dbg==1) llSay(0, text);
        else if(dbg==2) llShout(DEBUG_CHANNEL, text);
        else if(dbg<0) llShout(dbg,text);
    }
}

//==============> START Read Notecard for sim model
readfirstNCLine(string name) 
// Start reading a notecard line. Only call this when this notecard exists
{
    currentLine=0; 
    currentName=name;
    if (llGetInventoryType(name)!=INVENTORY_NOTECARD){
        llSay(0, "Unable to find the '"+name+"' notecard. Default settings will be loaded.");
        llListenRemove(pingHandle);
        pingHandle=llListen(pingChannel,"","","");
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

    string keystring=llList2String(parts, 0);
    keystring=llToLower(llStringTrim(keystring, STRING_TRIM));
    string value=llList2String(parts, 1);
    value=llStringTrim(value, STRING_TRIM);

    if (keystring=="pingchannel") {
        if ((integer)value!=0) pingChannel=(integer)value;
        return;
    }
    llSay(0, "Settings: Unknown keyword: \""+keystring+"\"");    
}
//<============== END Read Notecard

inicioSim() //rez object with normal line mode sim
{
    if(llGetInventoryType("StartLine settings_obj")!=INVENTORY_NONE){
        readfirstNCLine("StartLine settings_obj");
    }else{     
        llListenRemove(pingHandle);
        pingHandle=llListen(pingChannel,"","","");
    }
}

preinici(integer p)
{
    if(p>=10000) flyNum=(string)p;
    flyObjChannel=flyChannel-5;
    flyObjChannelin=flyObjChannel-1;
    llListenRemove(flylistenHandle);
    flylistenHandle=llListen(flyObjChannelin, "", "", "");
    flyObj=(string)(p%10);
    llShout(flyObjChannel,flyNum+"param");
}

inicio(string ps)
{
    list l=llCSV2List(ps);
    flySimPos=(vector)llList2String(l,0);
    flyPosition=(vector)llList2String(l,1);
    prePos=(vector)llList2String(l,3);
    prePosDir=(integer)llList2String(l,4);
    preRot=(rotation)llList2String(l,5);
    simDestName=llList2String(l,6);
    lineLen=(float)llList2String(l,7);
    flyLineRotation=(integer)llList2String(l,8);
    pingChannel=(integer)llList2String(l,9);
    flyObjPos=llList2String(l,10);
    flyObjOffset=(vector)llList2String(l,11);
    flyObjRot=llEuler2Rot((vector)llList2String(l,12)*DEG_TO_RAD);
    flyObjSize=(float)llList2String(l,13);
    flyObjPhantom=(integer)llList2String(l,14);    
    flyObjAlpha=(integer)llList2String(l,15);   
    flyObjBright=(integer)llList2String(l,16);   
    dbg=(integer)llList2String(l,17);
    //model=(integer)llList2String(l,18);
    stateline=(integer)llList2String(l,19);

    if(flyObjAlpha>0) llSetLinkAlpha(LINK_SET,0.0,ALL_SIDES);
    if(flyObjPhantom>0) llSetStatus(STATUS_PHANTOM,TRUE);
    integer n=1;
    float f;
    //put pre position
    vector v=prePos;
    if(dbg!=0) dbgSay(flyNum+" Set pre position "+(string)v+"   *"+simDestName+"*");
    llSetRot(preRot);
    if(llGetRegionName()==simDestName){
        prePos=llGetPos();
        postInicio();
    }else{
        if(prePos!=ZERO_VECTOR){
            n=30;
            while(n>0){
                llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_POSITION,v ]);
                f=llVecDist(llGetPos(),v);
                if(f<0.1) n=0; 
                else if(--n==0) errorDie();
            }
            llSleep(0.5);
            fshotLine();
        }
    }
}

postInicio()
{    
    if(dbg!=0) dbgSay((string)flyNum+" End position: "+(string)flyPosition);
    //ponerse en posicion
    vector vpos;
    integer n=1;
    float f;
    vector v;
    float radio;
    if(flyObjPos=="C") vpos=flyPosition+flyObjOffset;
    else{
        radio=lineLen/2;
        if(flyObjPos=="L"){
            vpos.x=flyPosition.x-radio*llCos(flyLineRotation*DEG_TO_RAD);
            vpos.y=flyPosition.y-radio*llSin(flyLineRotation*DEG_TO_RAD);
        }else{
            vpos.x=flyPosition.x+radio*llCos(flyLineRotation*DEG_TO_RAD);
            vpos.y=flyPosition.y+radio*llSin(flyLineRotation*DEG_TO_RAD);
        }
        vpos.z=flyPosition.z;
        vpos+=flyObjOffset;
    }
    v=<vpos.x,vpos.y,prePos.z>;
    n=30;
    while(n>0){
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_POSITION,v]);
        f=llVecDist(llGetPos(),v);
        if(f<0.1) n=0;   
        else n--;
    }
    
    n=30;
    while(n>0){
        llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_POSITION,vpos]);
        f=llVecDist(llGetPos(),vpos);
        if(f<0.1) n=0;   
        else n--;
    }
    //poner tamaño
    llSetRot(flyObjRot);
    if(flyObjSize>1.0) llScaleByFactor(flyObjSize);
    if(flyObjPhantom==1) llSetStatus(STATUS_PHANTOM,FALSE);
    if(flyObjBright==2){ 
        if(stateline==1) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
        else llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
    }else if(flyObjBright==1) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
    else if(flyObjBright==0) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
    llListenRemove(pingHandle);
    pingHandle=llListen(pingChannel,"","","");
    if(flyObjAlpha>0 && model!=1) llSetLinkAlpha(LINK_SET,flyObjAlpha,ALL_SIDES);
    if(dbg!=0) dbgSay((string)flyNum+" fin postinicio");
    llShout(flyObjChannel,flyNum+"rezed");
}


fshotLine() 
{
    llSetStatus(STATUS_PHYSICS,TRUE);
    llSetVehicleType(VEHICLE_TYPE_SLED);
    llSetVehicleFloatParam(VEHICLE_LINEAR_MOTOR_TIMESCALE,0.5);
    llSetVehicleVectorParam(VEHICLE_LINEAR_MOTOR_DIRECTION,<10,0,0>);
    llSetTimerEvent(1.0);
}

errorDie()
{
    llOwnerSay("An error has occurred and the object "+llGetObjectName()+" has been deleted");
    if(dbg!=0) dbgSay(flyNum+" An error has occurred and the object "+llGetObjectName()+" has been deleted");
    llDie();
}

setTexture(string ptex)  //change texture to StartLine Buoy
{
    list l=llCSV2List(ptex);
    key k;
    k=(key)llList2String(l,1);
    if(k) llSetLinkPrimitiveParamsFast(2,[PRIM_TEXTURE,1,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
    k=(key)llList2String(l,2);
    if(k) llSetLinkPrimitiveParamsFast(2,[PRIM_TEXTURE,2,k,<1.0,1.0,0.0>,<0.0,0.0,0.0>,0.0]);
    k=(key)llList2String(l,3);
    if(k) llSetLinkPrimitiveParamsFast(2,[PRIM_TEXTURE,0,k,<(float)llList2String(l,4),1.0,0.0>,<0.0,0.0,0.0>,0.0]);
}

default 
{
    state_entry()
    {
        flyNum="90000";
        llListenRemove(pingHandle);
        pingHandle=llListen(pingChannel,"","","");
    }
    
    listen(integer pchannel, string name, key id, string cmd)  
    {
        if(pchannel==flyObjChannelin){
            //llOwnerSay("** StartLine listen "+flyNum+" "+cmd);
            string scmd=llGetSubString(cmd,5,9);
            if(llGetSubString(scmd,0,2)=="die"){
                if((key)llGetSubString(cmd,11,-1)!=llGetOwner()) return;
            }else if(model==0){
                if(llGetOwnerKey(id)!=llGetOwner()) return;
            }
            if(llGetSubString(cmd,0,4)==flyNum){
                if(scmd=="dieee"){     //borra el numero enviado
                    if(dbg!=0) dbgSay(flyNum+"    Dieee");
                    llDie(); 
                }else if(scmd=="param"){ 
                    inicio(llGetSubString(cmd,11,-1)); //**
                }else if(scmd=="textu"){ 
                    if(llGetSubString(llGetObjectName(),0,16)=="StartLine RezBuoy") setTexture(cmd);
                }
            }else if(llGetSubString(cmd,0,3)==llGetSubString(flyNum,0,3)){
                if(scmd=="dieen"){     //borra todo el grupo del numero enviado
                    if(dbg!=0) dbgSay(flyNum+"    Dieen");
                    llDie(); 
                }
                if(scmd=="ready") llSetLinkAlpha(LINK_SET,flyObjAlpha,ALL_SIDES);
            }else if(llGetSubString(cmd,0,4)!=flyNum){
                if(scmd=="dienn" && flyNum!="9000"){   //borra todos menos el numero enviado 
                    if(dbg!=0) dbgSay(flyNum+"    Dienn");
                    llDie(); 
                }
            }
        }else if(pchannel==pingChannel){
            if(model==0) if(llGetOwnerKey(id)!=llGetOwner()) return;
            if(llGetSubString(cmd,0,6)=="visline"){
                if(flyObjAlpha>0){
                    if(llGetSubString(cmd,8,8)=="0") llSetLinkAlpha(LINK_SET,0.0,ALL_SIDES);
                    else llSetLinkAlpha(LINK_SET,flyObjAlpha,ALL_SIDES);
                }
                return;
            }
            if(llGetSubString(cmd,0,4)=="start"){
                if(flyObjBright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
                return;
            }
            if(llGetSubString(cmd,0,6)=="restart"){
                if(flyObjBright==2) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
                return;
            }
            if(llGetSubString(cmd,0,5)=="bright"){
                if(model==0){   
                    flyObjBright=(integer)llGetSubString(cmd,7,7);
                    if(flyObjBright==2 || flyObjBright==0) llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,0]);
                    else llSetLinkPrimitiveParamsFast(LINK_SET,[PRIM_FULLBRIGHT,ALL_SIDES,1]);
                }
                return;
            }
        }
    }

    on_rez(integer whocares)
    {
        flyNum="90000";
        if(whocares>=10000){
            model=1;
            preinici(whocares);
        }else{
            model=0;
            inicioSim();
        }
    }
    
    dataserver(key queryid, string data) 
    {
        if (queryid == requestInFlight){
            if (data == EOF){
                requestInFlight = NULL_KEY;
                llListenRemove(pingHandle);
                pingHandle=llListen(pingChannel,"","","");
            }else{
                readNextNCLine(data);
            }
        }        
    }
    
    timer(){
        llSetTimerEvent(0.0);
        llSetStatus(STATUS_PHYSICS,FALSE);
        llSetVehicleType(VEHICLE_TYPE_NONE);
        if(llGetRegionName()==simDestName) postInicio();
        else{
            llOwnerSay("An error has occurred and the object "+llGetObjectName()+" has been deleted");
            if(dbg!=0) dbgSay(flyNum+"An error has occurred and the object "+llGetObjectName()+" has been deleted");
            llDie();
        }
    }
}