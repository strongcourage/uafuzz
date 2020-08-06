ocaml:  
	piqi of-proto -I ./ common.proto -o ../piqi/common.proto.piqi
	piqic-ocaml --multi-format ../piqi/common.proto.piqi -C ../piqi
	piqi of-proto -I ./ libcall.proto -o ../piqi/libcall.proto.piqi
	piqic-ocaml --multi-format ../piqi/libcall.proto.piqi -C ../piqi
	piqi of-proto -I ./ syscall.proto -o ../piqi/syscall.proto.piqi
	piqic-ocaml --multi-format ../piqi/syscall.proto.piqi -C ../piqi
	piqi of-proto -I ./ dba.proto -o ../piqi/dba.proto.piqi
	piqic-ocaml --multi-format ../piqi/dba.proto.piqi -C ../piqi
	piqi of-proto -I ./ analysis_config.proto -o ../piqi/analysis_config.proto.piqi
	piqic-ocaml --multi-format ../piqi/analysis_config.proto.piqi -C ../piqi
	piqi of-proto -I ./ trace.proto -o ../piqi/trace.proto.piqi
	piqic-ocaml --multi-format ../piqi/trace.proto.piqi -C ../piqi
	piqi of-proto -I ./ instruction.proto -o ../piqi/instruction.proto.piqi
	piqic-ocaml --multi-format ../piqi/instruction.proto.piqi -C ../piqi
	piqi of-proto -I ./ config.proto -o ../piqi/config.proto.piqi
	piqic-ocaml --multi-format ../piqi/config.proto.piqi -C ../piqi
	piqi of-proto -I ./ message.proto -o ../piqi/message.proto.piqi
	piqic-ocaml --multi-format ../piqi/message.proto.piqi -C ../piqi

cpp:
	protoc --proto_path=./ --cpp_out=../../pinsec/src/types/protobuf common.proto dba.proto analysis_config.proto config.proto instruction.proto syscall.proto libcall.proto message.proto trace.proto

python:
	protoc --proto_path=./ --python_out=../python/proto common.proto dba.proto analysis_config.proto config.proto instruction.proto syscall.proto libcall.proto message.proto trace.proto
