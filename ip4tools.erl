-module(ip4tools).
-export([ip_to_number/1, number_to_ip/1]).
-export([get_network/2]).
-export([calculate_network/2]).

ip_to_number(IP) when list(IP) ->
  {_, Parts} = regexp:split(IP, "\\."),
  encode_ip([convert_part(P) || P <- Parts]);
ip_to_number(_IP) -> 0.


number_to_ip(Num) when integer(Num) ->
  Octets = decode_ip(Num),
  make_ip_string(Octets).


encode_ip(Octets) when is_list(Octets) ->
  encode_ip(Octets, 0).

encode_ip([], IPNum) -> IPNum;
encode_ip([Oct|Rest], IPNum) ->
  Num = IPNum bsl 8,
  encode_ip(Rest, Num bor Oct).


decode_ip(Num) -> decode_ip(Num, []).

decode_ip(0, Octets) -> Octets;
decode_ip(Num, Octets) ->
  Oct = Num band 255,
  Rest = Num bsr 8,
  decode_ip(Rest, [Oct | Octets]).


convert_part(P) when list(P) ->
  {I, _} = string:to_integer(P), I;
convert_part(P) when integer(P) -> P;
convert_part(_P) -> 0.


make_ip_string([H|T]) ->
  IpStr = string:concat(integer_to_list(H), "."),
  make_ip_string(IpStr, T).

make_ip_string(Current, []) -> Current;
make_ip_string(Current, [H|T]) ->
  NewStr = string:concat(Current, integer_to_list(H)),
  if
    length(T) == 0 -> NewStr;
    true ->
      IpStr  = string:concat(NewStr, "."),
      make_ip_string(IpStr, T)
  end.


get_network(IP, Subnet) ->
  IPNum = ip_to_number(IP),
  SubnetNum = ip_to_number(Subnet),
  Host = IPNum band SubnetNum,
  number_to_ip(Host).


calculate_network(IP, Subnet) ->
  SubnetNum = ip_to_number(Subnet),
  Network = get_network(IP, Subnet),
  NetworkNum = ip_to_number(Network),
  NumHosts = round(math:pow(2, (32 - count_bits(SubnetNum)))) - 2,
  Hosts = get_hosts(NetworkNum, NumHosts),
  BroadcastNum = NetworkNum + NumHosts + 1,
  Broadcast = number_to_ip(BroadcastNum),
  {Network, Broadcast, Hosts}.


count_bits(Num) -> count_bits(Num, 0).

count_bits(0, Count) -> Count;    
count_bits(Num, Count) ->
  NewNum = Num bsr 1,
  if
    Num rem 2 == 0 -> count_bits(NewNum, Count);
    true -> count_bits(NewNum, Count + 1)
  end.


get_hosts(Host, Count) ->
  NewCount = Count - 1,
  if
    NewCount < 0 -> [];
    true ->
      IP = number_to_ip(Host + 1),
      [IP | get_hosts(Host + 1, NewCount)]
  end.

