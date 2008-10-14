module KeyHelpTable(keyHelpTable) where

keyHelpTable gk = [
	 Left "Main Screens"
	,Right (gk "show_help_screen", "Show help screen")
	,Right (gk "show_presence_status", "show presence status")
	,Right (gk "show_puff_details", "show puff details")
	,Right (gk "show_status_screen", "show status screen")
	,Left "Puff Movement"
	,Right (gk "next_puff", "Goto the next puff")
	,Right (gk "previous_puff", "Goto the previous puff")
	,Right (gk "first_puff", "Goto the first puff")
	,Right (gk "last_puff", "Goto the last puff")
	,Right (gk "next_line", "Scroll one line down")
	,Right (gk "previous_line", "Scroll one line up")
	,Right (gk "next_page", "Scroll one page down")
	,Right (gk "previous_page", "Scroll one page up")
	,Right (gk "forward_half_page", "Scroll a half page down")
	,Right (gk "backward_half_page", "Scroll a half page up")
	,Left "Filter Manipulation"
	,Right (gk "prompt_new_filter", "prompt new filter")
	,Right (gk "prompt_new_filter_slash", "prompt new filter slash")
	,Right (gk "prompt_new_filter_twiddle", "prompt new filter twiddle")
	,Right (gk "pop_one_filter", "pop one filter")
	,Right (gk "pop_all_filters", "pop all filters")
	,Right (gk "invert_filter", "invert filter")
	,Right (gk "filter_current_thread", "filter current thread")
	,Right (gk "swap_filters", "swap filters")
	,Right (gk "filter_current_author", "filter current author")
	,Left "Puff Body Filtering"
	,Right (gk "toggle_rot13", "toggle Rot13 filter")
	,Left "Mark/Workspace Manipulation"
	,Right (gk "set_mark", "save current position at a given mark.")
	,Right (gk "recall_mark", "goto position saved at mark")
	,Right (gk "set_filter_mark", "save current filter stack at given mark.")
	,Right (gk "recall_filter_mark", "Recall filter stack at mark")
	,Right (gk "recall_combine_mark", "Recall filter stack at mark and combine it with the current filter stack.")
	,Right ("[1-9]", "Recall given numbered workspace and set it as current workspace, these follow marks [1-9]")
	,Left "Composing Puffs"
	,Right (gk "new_puff", "compose a new puff")
	,Right (gk "follow_up", "follow up to the same category as the current puff")
	,Right (gk "reply_to_author", "reply to the author of the current puff privatly")
	,Right (gk "group_reply", "reply to the union of the sender and categories of the current puff")
	,Right (gk "resend_puff", "resend puff")
	,Left "Miscellaneous"
	,Right (gk "goto_match", "visit link in current puff")
	,Right (gk "modify_presence_string", "modify presence string")
	,Right (gk "reconnect_to_servers", "reconnect to servers")
	,Right (gk "edit_config_file", "Edit the configuration file and reload its settings")
	,Right (gk "ask_quit", "quit ginsu")
	,Right (gk "fast_quit", "quit ginsu without asking first")
	,Right (gk "redraw_screen", "redraw screen")
	]
