integer updtextChannel=         //***** Put channel, Communication channel with the design kit 
integer listenHandle;
string notename="StartLine texture updater data";
integer currentLine;
key requestInFlight;
string currentName=" ";
string chrono_title=" ";
string chrono_numbers=" ";
string line_main=" ";
string line_arrow=" ";
string main_scale="1";
string arrow_scale="1";

readfirstNCLine(string name) 
// Start reading a notecard line. Only call this when this notecard exists
{
    chrono_title=" ";
    chrono_numbers=" ";
    line_main=" ";
    line_arrow=" ";
    main_scale="1";
    arrow_scale="1";
    currentLine=0; 
    currentName="";
    integer n=llGetInventoryNumber(INVENTORY_NOTECARD);
    integer len=llStringLength(name);
    integer i;
    for (i=0;i<n;i++) {
        if (llGetSubString(llGetInventoryName(INVENTORY_NOTECARD,i),0,len-1)==name) {
            currentName=llGetInventoryName(INVENTORY_NOTECARD,i);
        }
    }
    if (currentName=="") {
        llOwnerSay("Unable to find the settings notecard");
    }else{ 
        llOwnerSay("Loading Notecard");
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

    if (keystring=="chrono_title") {
        if (value!=""){ 
            if((key)value) chrono_title=value;
        }
        return;
    }
    if (keystring=="chrono_numbers") {
        if (value!=""){ 
            if((key)value) chrono_numbers=value;
        }
        return;
    }
    if (keystring=="line_main") {
        if (value!=""){ 
            if((key)value) line_main=value;
        }
        return;
    }
    if (keystring=="line_arrow") {
        if (value!=""){ 
            if((key)value) line_arrow=value;
        }
        return;
    }
    if (keystring=="main_scale") {
        if (value!=""){ 
            if((integer)main_scale<1 || (integer)main_scale>20) main_scale="1";
            else main_scale=value;
        }
        return;
    }
    if (keystring=="arrow_scale") {
        if (value!=""){ 
            if((integer)arrow_scale<1 || (integer)arrow_scale>20) arrow_scale="1";
            else arrow_scale=value;
        }            
        return;
    }
    llOwnerSay("Settings updater: Unknown keyword: \""+keystring+"\"");    
}

default
{
    state_entry()
    {
        listenHandle = llListen(updtextChannel, "", "", "");
    }
    
    dataserver( key queryid, string data ) 
    {
        if (queryid == requestInFlight){
            if (data == EOF){
                requestInFlight = NULL_KEY;
                if (currentName!="") llRemoveInventory(currentName);
                llOwnerSay("Loaded Notecard");
            }else{
                readNextNCLine(data);
            }
        }        
    }
    
    changed(integer change)
    {
        if (change & CHANGED_INVENTORY)         
        {
            readfirstNCLine(notename);
        }
    }    

    listen(integer pchannel, string name, key id, string cmd) 
    {
        list l=llParseString2List(cmd,["|"],[]);
        if (llList2String(l,0)=="updtextreq") {
            if ((key)llList2String(l,1)==llGetOwner()){
                llOwnerSay("Updating Start Line ....");
                string s=chrono_title+"|"+chrono_numbers+"|"+line_main+"|"+line_arrow+"|"+main_scale+"|"+arrow_scale;
                llRegionSayTo(id,updtextChannel,"updtextans|"+s);
            }
        }
    }
}
