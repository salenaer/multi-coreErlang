for i in `seq 20 5 64`
do  
	echo "---"
	echo "> send_message_random_sparce, $i threads"
	erl +S $i -noshell -s benchmark send_message_random_sparce -s init stop > testResults/random/sparce/output-$i.txt
	echo "---"
	echo "> send_message_random_dense, $i threads"
	erl +S $i -noshell -s benchmark send_message_random_dense -s init stop > testResults/random/dense/output-$i.txt
	echo "---"
	echo "> send_message_structured_sparce, $i threads"
	erl +S $i -noshell -s benchmark send_message_random_sparce -s init stop > testResults/structured/sparce/output-$i.txt
	echo "---"
	echo "> send_message_structured_dense, $i threads"
	erl +S $i -noshell -s benchmark send_message_random_sparce -s init stop > testResults/structured/dense/output-$i.txt
	echo "---"
	echo "> send_message_worst_case, $i threads"
	erl +S $i -noshell -s benchmark send_message_random_sparce -s init stop > testResults/worstcase/output-$i.txt
	echo "> latency, $i threads"
	erl +S $i -noshell -s benchmark measure_latency -s init stop > testResults/latency/output-$i.txt
	echo "> channel_history, $i threads"
	erl +S $i -noshell -s benchmark measure_channel_history -s init stop > testResults/history/output-$i.txt
done