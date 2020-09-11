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
//About this script - StartLine Chrono


//Texture Parameters sign and numbers
float charsize=0.25; //character width and height
vector charscale=<1.0,1.0,0.0>; //scale character
key tex;  //key texture
//faces number
integer TITLE=0;
integer SIGN=1;
integer HOUR=2;
integer COLON=3;
integer MIN10=4;
integer MIN1=5;
integer SEC10=6;
integer SEC1=7;
//others
string SIMBOLS="0123456789-+ '.:";


integer n0; integer n1; integer n2; integer n3; integer n4; integer n5;
integer i;
integer j;
integer length;
string letter;
key self;
string timeStr;
integer time;
integer startTime;
integer hours;
integer minutes;
integer seconds;
integer start_at;
integer visible=1;

//colors
integer colorchrono = 1;
vector color61 = <0,0,1>;  //blue
vector color60 = <1,1,0>;  //yellow
vector color30 = <1,0.5,0>; //orange
vector color10 = <1,0,0>; //red
vector colorstart = <0,1,0>; //green
vector colornormal = <0.539,0.6679,1.0>; //light blue

displayChrono(){
    integer now=llGetUnixTime();
    time=now-start_at;
    displayTime(1,time);
}

displayTime(integer pmode, integer ptime) {
    if (pmode==1 && colorchrono) {
        if (ptime<0) {
            if (ptime<-60) changeColor(color61);
            else if (ptime<-30) changeColor(color60);
            else if (ptime<-10) changeColor(color30);
            else changeColor(color10);
        } else {
            changeColor(colorstart);
        }
    } else {
        changeColor(colornormal);
    }
    if (ptime<0) n0=llSubStringIndex(SIMBOLS,"-");
    else n0=llSubStringIndex(SIMBOLS,"+");
    ptime=llAbs(ptime);
    hours=ptime/3600;
    ptime-=hours*3600;
    minutes=ptime/60;
    seconds=ptime%60;
    n1=llSubStringIndex(SIMBOLS,(string)(hours%10));
    n2=llSubStringIndex(SIMBOLS,(string)(minutes/10));
    n3=llSubStringIndex(SIMBOLS,(string)(minutes%10));
    n4=llSubStringIndex(SIMBOLS,(string)(seconds/10));
    n5=llSubStringIndex(SIMBOLS,(string)(seconds%10));
    llSetLinkPrimitiveParamsFast(LINK_THIS,[
        PRIM_TEXTURE,SIGN,tex,charscale,<(n0%4)*charsize, (3-n0/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,HOUR,tex,charscale,<(n1%4)*charsize, (3-n1/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,MIN10,tex,charscale,<(n2%4)*charsize, (3-n2/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,MIN1,tex,charscale,<(n3%4)*charsize, (3-n3/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SEC10,tex,charscale,<(n4%4)*charsize, (3-n4/4)*charsize,0.0>,0.0,
        PRIM_TEXTURE,SEC1,tex,charscale,<(n5%4)*charsize, (3-n5/4)*charsize,0.0>,0.0
    ]);
}

changeColor(vector colorVec) {
    llSetLinkPrimitiveParamsFast(LINK_THIS,[PRIM_COLOR,SIGN,colorVec,visible,PRIM_COLOR,HOUR,colorVec,visible,PRIM_COLOR,MIN1,colorVec,visible,PRIM_COLOR,MIN10,colorVec,visible,PRIM_COLOR,SEC1,colorVec,visible,PRIM_COLOR,SEC10,colorVec,visible,PRIM_COLOR,COLON,colorVec,visible]);
}

default {
    
    state_entry() {
        //llListen(5910798,"",NULL_KEY,"");
        startTime=0;
        time=startTime;

        //llSetStatus(STATUS_PHANTOM,TRUE);
        llSetTimerEvent(0);
        //llOwnerSay(llGetScriptName()+"  "+(string)llGetFreeMemory());
    }
    
    //listen(integer channel,string name,key id,string message ) {
    //    displayTime((integer)message);
    //}
 
    timer() {
        displayChrono();
    }
    
    link_message(integer sender_num,integer num,string str,key id) {
        if(str=="init"){
            time=num;
            displayTime(0,time);
            return;
        }
        if(str=="start"){
            start_at=num;
            displayChrono();
            llSetTimerEvent(1);
            return;
        }
        if(str=="restart"){
            time=num;
            displayTime(0,time);
            changeColor(colornormal);
            llSetTimerEvent(0);
            return;
        }
        if(llGetSubString(str,0,5)=="colors"){
            list l=llParseString2List(str,["|"],[]);
            colorchrono = (integer)llList2String(l,1);
            color61 = (vector)llList2String(l,2);
            color60 = (vector)llList2String(l,3);
            color30 = (vector)llList2String(l,4);
            color10 = (vector)llList2String(l,5);
            colorstart = (vector)llList2String(l,6);
            colornormal = (vector)llList2String(l,7);
            return;
        }
        if(str=="reset"){
            llResetScript();
            return;
        }
        if(str=="visible"){  
            if(num==0) visible=0;
            else visible=1;
            return;
        }
        if(llGetSubString(str,0,12)=="updtextchrono"){
            tex=(key)llGetSubString(str,14,-1);
            displayTime(0,time);
        } 
    }
        
}
