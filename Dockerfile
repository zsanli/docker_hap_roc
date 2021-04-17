FROM germline_exome_trio_pipeline_combine_hap_base:v0.1
LABEL maintainer="zhi.zhang@lns.etat.lu"
USER   velona
WORKDIR /home/velona
VOLUME /mnt/gen_bioinfassets
VOLUME  /mnt/gen_bioinfdata
VOLUME  /mnt/gen_bioinfinternal
ENV HTTP_PROXY "http://proxy.etat.lu:80"
ENV HTTPS_PROXY "http://proxy.etat.lu:80"
ENV http_proxy "http://proxy.etat.lu:80"
ENV https_proxy "http://proxy.etat.lu:80"
ENV NO_PROXY "127.0.0.1,localhost,*.lns.lu"
COPY ./hap.py /home/velona
CMD ["bash"] 