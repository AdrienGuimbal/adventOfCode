input="iwrupvqb"
i=0
while [[ "$(echo "$input$i" | md5sum)" == "00000*" ]] do
	i=eval "$i+1"
done
