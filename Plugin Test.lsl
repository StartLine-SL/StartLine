key k;

default
{
    state_entry()
    {
        
    }

    link_message(integer sender_num, integer num, string str, key id)
    {
        if(num<=-20000){
            if(num==-20000){ 
                llMessageLinked(LINK_THIS, 21010, "1XX", "");
                llMessageLinked(LINK_THIS, 21011, "1", "");    //set receive link with num -21011 when select race mode option menu
                llMessageLinked(LINK_THIS, 21047, "1", "plugin 1");
                llMessageLinked(LINK_THIS, 21047, "1", "plugin 2");
                llMessageLinked(LINK_THIS, 21047, "1", "plugin 3");
                return;
            }
            if(num==-21047){ 
                llOwnerSay(str);
            }
            return;
        }
    }
    
    timer()
    {
        llSetTimerEvent(0);
        //llMessageLinked(LINK_THIS, 21010, "201", k);
    }

}
