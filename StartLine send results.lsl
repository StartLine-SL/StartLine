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
integer region_relay_channel;
string name;

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

default {
    link_message(integer sender_num,integer num,string str,key data) {
        if(str=="lapsadd"){
            allLapsList+=["r"+(string)num,(integer)llGetSubString(data,0,0),(integer)llGetSubString(data,2,-1)];
            return;
        }        
        if(str=="lapssay"){
            string s="\nLap times";
            integer idx;
            integer y;
            integer raceNumLaps=(integer)llGetSubString(data,0,0);
            integer listIdx=num;
            for (y=0;y<=raceNumLaps;++y) { 
                idx=llListFindList(allLapsList,["r"+(string)listIdx,y]);
                if (idx>=0) {
                    if (y==0) s+=" - Start: "+sec2hhmmss(llList2Integer(allLapsList,idx+2)); 
                    else if(y==raceNumLaps)  s+=" - Last lap: "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                    else s+=" - Lap "+(string)y+": "+sec2hhmmss(llList2Integer(allLapsList,idx+2));
                }
            }
            s=llGetSubString(data,2,-1)+s;
            llShout(0,"/me : "+s); 
            if(region_relay_channel!=0) llRegionSay(region_relay_channel, s);
            return;
        }
        if(str=="lapsini"){
            allLapsList=[];
            return;
        }
        if(llGetSubString(str,0,7)=="results|"){
            llSetObjectName(" ");
            llShout(0,"/me "+llGetSubString(str,8,-1));
            llSetObjectName(name);
            return;
        }
        if(str=="config"){
            region_relay_channel=num;
            return;
        }        
        if(str=="name"){
            name=data;
            llSetObjectName(name);
            return;
        }        
        if(str=="reset"){
            llResetScript();
            return;
        }        
    }
}
