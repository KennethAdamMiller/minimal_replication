#pushd plugin/
rm -rf _build
#-pkg superset_disassemblers 
bapbuild -pkg findlib.dynload -pkg bap-primus -pkg bap-knowledge -pkg minimal_replication demo.plugin
#popd
