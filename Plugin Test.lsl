default
{
    state_entry()
    {
        
    }

    link_message (integer sender_num, integer num, string str, key id)
    {
        if (num == -20000) {
            llMessageLinked (LINK_THIS, 21011, "1", "");   //prepared to receive call with num -21011 when select race mode option menu
            return;
        }

        if (num == -21011) {   //when select race mode option menu you receive this message
            // your code ...
            return;
        }
    }
}
