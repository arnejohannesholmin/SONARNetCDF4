# Definition of the SingleTarget sub-group of the Beam_groupX sub-group of the Sonar group:
getSingleTargetSchema <- function(data) {
	
	groupName <- "/Sonar/Beam_group1/SingleTarget"
	
	dimension_variables <- c(
		"ping_time", 
		"beam", 
		"frequency"
	)
	if(!missing(data)) {
		dimension_variables <- intersect(dimension_variables, names(data))
	}
	
	variables <- c(
		"single_target_count", 
		
		"single_target_identifier", 
		
		"single_target_range", 
		"single_target_start_range", 
		"single_target_stop_range", 
		
		"single_target_alongship_angle", 
		"single_target_athwartship_angle", 
		
		"compensated_TS", 
		"uncompensated_TS", 
		
		"single_target_detection_algorithm", 
		
		"param_TS_threshold", 
		"param_gain_compensation", 
		"param_minimum_echo_duration", 
		"param_maximum_echo_duration", 
		"param_maximum_phase_deviation", 
		"param_minimum_echo_spacing", 
		"param_TSf_processing_window_duration"
	)
	if(!missing(data)) {
		variables <- intersect(names(data), variables)
	}
	
	vlen_variables <- c(
		"single_target_identifier", 
		"single_target_range", 
		"single_target_start_range", 
		"single_target_stop_range", 
		"single_target_alongship_angle", 
		"single_target_athwartship_angle", 
		"compensated_TS", 
		"uncompensated_TS"
	)
	if(!missing(data)) {
		vlen_variables <- intersect(names(data), vlen_variables)
	}
	
	dimensions <- list(
		single_target_count = c("ping_time", "beam"), 
		single_target_identifier = c("ping_time", "beam"), 
		single_target_range = c("ping_time", "beam"), 
		single_target_start_range = c("ping_time", "beam"), 
		single_target_stop_range = c("ping_time", "beam"), 
		single_target_alongship_angle = c("ping_time", "beam"), 
		single_target_athwartship_angle = c("ping_time", "beam"), 
		compensated_TS = c("ping_time", "beam", "frequency"), 
		uncompensated_TS = c("ping_time", "beam", "frequency"), 
		single_target_detection_algorithm = c("ping_time", "beam"), 
		param_TS_threshold = c("ping_time", "beam"), 
		param_gain_compensation = c("ping_time", "beam"), 
		param_minimum_echo_duration = c("ping_time", "beam"), 
		param_maximum_echo_duration = c("ping_time", "beam"), 
		param_maximum_phase_deviation = c("ping_time", "beam"), 
		param_minimum_echo_spacing = c("ping_time", "beam"), 
		param_TSf_processing_window_duration = c("ping_time", "beam")
	)
	if(!missing(data)) {
		dimensions <- subset(dimensions, names(dimensions) %in% names(data))
	}
	
	out <- list(
		groupName = groupName, 
		dimension_variables = dimension_variables,
		variables = variables,
		vlen_variables = vlen_variables,
		dimensions = dimensions
	)
	
	
	return(out)
}

