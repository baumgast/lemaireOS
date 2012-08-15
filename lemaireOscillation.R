lemaire = function(Time, State, Pars){
  with(as.list(c(State,Pars)),{
    
    dx1  =  a200*x200	-	a1*x1
    dx2  =  a1	*	x1	-	a2	*	x2
    dx3	=	a2	*	x2	-	a3	*	x3
    dx4	=	a3	*	x3	-	a4	*	x4
    dx5	=	a4	*	x4	-	a5	*	x5
    dx6	=	a5	*	x5	-	a6	*	x6
    dx7	=	a6	*	x6	-	a7	*	x7
    dx8	=	a7	*	x7	-	a8	*	x8
    dx9	=	a8	*	x8	-	a9	*	x9
    dx10	=	a9	*	x9	-	a10	*	x10
    dx11	=	a10	*	x10	-	a11	*	x11
    dx12	=	a11	*	x11	-	a12	*	x12
    dx13	=	a12	*	x12	-	a13	*	x13
    dx14	=	a13	*	x13	-	a14	*	x14
    dx15	=	a14	*	x14	-	a15	*	x15
    dx16	=	a15	*	x15	-	a16	*	x16
    dx17	=	a16	*	x16	-	a17	*	x17
    dx18	=	a17	*	x17	-	a18	*	x18
    dx19	=	a18	*	x18	-	a19	*	x19
    dx20	=	a19	*	x19	-	a20	*	x20
    dx21	=	a20	*	x20	-	a21	*	x21
    dx22	=	a21	*	x21	-	a22	*	x22
    dx23	=	a22	*	x22	-	a23	*	x23
    dx24	=	a23	*	x23	-	a24	*	x24
    dx25	=	a24	*	x24	-	a25	*	x25
    dx26	=	a25	*	x25	-	a26	*	x26
    dx27	=	a26	*	x26	-	a27	*	x27
    dx28	=	a27	*	x27	-	a28	*	x28
    dx29	=	a28	*	x28	-	a29	*	x29
    dx30	=	a29	*	x29	-	a30	*	x30
    dx31	=	a30	*	x30	-	a31	*	x31
    dx32	=	a31	*	x31	-	a32	*	x32
    dx33	=	a32	*	x32	-	a33	*	x33
    dx34	=	a33	*	x33	-	a34	*	x34
    dx35	=	a34	*	x34	-	a35	*	x35
    dx36	=	a35	*	x35	-	a36	*	x36
    dx37	=	a36	*	x36	-	a37	*	x37
    dx38	=	a37	*	x37	-	a38	*	x38
    dx39	=	a38	*	x38	-	a39	*	x39
    dx40	=	a39	*	x39	-	a40	*	x40
    dx41	=	a40	*	x40	-	a41	*	x41
    dx42	=	a41	*	x41	-	a42	*	x42
    dx43	=	a42	*	x42	-	a43	*	x43
    dx44	=	a43	*	x43	-	a44	*	x44
    dx45	=	a44	*	x44	-	a45	*	x45
    dx46	=	a45	*	x45	-	a46	*	x46
    dx47	=	a46	*	x46	-	a47	*	x47
    dx48	=	a47	*	x47	-	a48	*	x48
    dx49	=	a48	*	x48	-	a49	*	x49
    dx50	=	a49	*	x49	-	a50	*	x50
    dx51	=	a50	*	x50	-	a51	*	x51
    dx52	=	a51	*	x51	-	a52	*	x52
    dx53	=	a52	*	x52	-	a53	*	x53
    dx54	=	a53	*	x53	-	a54	*	x54
    dx55	=	a54	*	x54	-	a55	*	x55
    dx56	=	a55	*	x55	-	a56	*	x56
    dx57	=	a56	*	x56	-	a57	*	x57
    dx58	=	a57	*	x57	-	a58	*	x58
    dx59	=	a58	*	x58	-	a59	*	x59
    dx60	=	a59	*	x59	-	a60	*	x60
    dx61	=	a60	*	x60	-	a61	*	x61
    dx62	=	a61	*	x61	-	a62	*	x62
    dx63	=	a62	*	x62	-	a63	*	x63
    dx64	=	a63	*	x63	-	a64	*	x64
    dx65	=	a64	*	x64	-	a65	*	x65
    dx66	=	a65	*	x65	-	a66	*	x66
    dx67	=	a66	*	x66	-	a67	*	x67
    dx68	=	a67	*	x67	-	a68	*	x68
    dx69	=	a68	*	x68	-	a69	*	x69
    dx70	=	a69	*	x69	-	a70	*	x70
    dx71	=	a70	*	x70	-	a71	*	x71
    dx72	=	a71	*	x71	-	a72	*	x72
    dx73	=	a72	*	x72	-	a73	*	x73
    dx74	=	a73	*	x73	-	a74	*	x74
    dx75	=	a74	*	x74	-	a75	*	x75
    dx76	=	a75	*	x75	-	a76	*	x76
    dx77	=	a76	*	x76	-	a77	*	x77
    dx78	=	a77	*	x77	-	a78	*	x78
    dx79	=	a78	*	x78	-	a79	*	x79
    dx80	=	a79	*	x79	-	a80	*	x80
    dx81	=	a80	*	x80	-	a81	*	x81
    dx82	=	a81	*	x81	-	a82	*	x82
    dx83	=	a82	*	x82	-	a83	*	x83
    dx84	=	a83	*	x83	-	a84	*	x84
    dx85	=	a84	*	x84	-	a85	*	x85
    dx86	=	a85	*	x85	-	a86	*	x86
    dx87	=	a86	*	x86	-	a87	*	x87
    dx88	=	a87	*	x87	-	a88	*	x88
    dx89	=	a88	*	x88	-	a89	*	x89
    dx90	=	a89	*	x89	-	a90	*	x90
    dx91	=	a90	*	x90	-	a91	*	x91
    dx92	=	a91	*	x91	-	a92	*	x92
    dx93	=	a92	*	x92	-	a93	*	x93
    dx94	=	a93	*	x93	-	a94	*	x94
    dx95	=	a94	*	x94	-	a95	*	x95
    dx96	=	a95	*	x95	-	a96	*	x96
    dx97	=	a96	*	x96	-	a97	*	x97
    dx98	=	a97	*	x97	-	a98	*	x98
    dx99	=	a98	*	x98	-	a99	*	x99
    dx100	=	a99	*	x99	-	a100	*	x100
    dx101	=	a100	*	x100	-	a101	*	x101
    dx102	=	a101	*	x101	-	a102	*	x102
    dx103	=	a102	*	x102	-	a103	*	x103
    dx104	=	a103	*	x103	-	a104	*	x104
    dx105	=	a104	*	x104	-	a105	*	x105
    dx106	=	a105	*	x105	-	a106	*	x106
    dx107	=	a106	*	x106	-	a107	*	x107
    dx108	=	a107	*	x107	-	a108	*	x108
    dx109	=	a108	*	x108	-	a109	*	x109
    dx110	=	a109	*	x109	-	a110	*	x110
    dx111	=	a110	*	x110	-	a111	*	x111
    dx112	=	a111	*	x111	-	a112	*	x112
    dx113	=	a112	*	x112	-	a113	*	x113
    dx114	=	a113	*	x113	-	a114	*	x114
    dx115	=	a114	*	x114	-	a115	*	x115
    dx116	=	a115	*	x115	-	a116	*	x116
    dx117	=	a116	*	x116	-	a117	*	x117
    dx118	=	a117	*	x117	-	a118	*	x118
    dx119	=	a118	*	x118	-	a119	*	x119
    dx120	=	a119	*	x119	-	a120	*	x120
    dx121	=	a120	*	x120	-	a121	*	x121
    dx122	=	a121	*	x121	-	a122	*	x122
    dx123	=	a122	*	x122	-	a123	*	x123
    dx124	=	a123	*	x123	-	a124	*	x124
    dx125	=	a124	*	x124	-	a125	*	x125
    dx126	=	a125	*	x125	-	a126	*	x126
    dx127	=	a126	*	x126	-	a127	*	x127
    dx128	=	a127	*	x127	-	a128	*	x128
    dx129	=	a128	*	x128	-	a129	*	x129
    dx130	=	a129	*	x129	-	a130	*	x130
    dx131	=	a130	*	x130	-	a131	*	x131
    dx132	=	a131	*	x131	-	a132	*	x132
    dx133	=	a132	*	x132	-	a133	*	x133
    dx134	=	a133	*	x133	-	a134	*	x134
    dx135	=	a134	*	x134	-	a135	*	x135
    dx136	=	a135	*	x135	-	a136	*	x136
    dx137	=	a136	*	x136	-	a137	*	x137
    dx138	=	a137	*	x137	-	a138	*	x138
    dx139	=	a138	*	x138	-	a139	*	x139
    dx140	=	a139	*	x139	-	a140	*	x140
    dx141	=	a140	*	x140	-	a141	*	x141
    dx142	=	a141	*	x141	-	a142	*	x142
    dx143	=	a142	*	x142	-	a143	*	x143
    dx144	=	a143	*	x143	-	a144	*	x144
    dx145	=	a144	*	x144	-	a145	*	x145
    dx146	=	a145	*	x145	-	a146	*	x146
    dx147	=	a146	*	x146	-	a147	*	x147
    dx148	=	a147	*	x147	-	a148	*	x148
    dx149	=	a148	*	x148	-	a149	*	x149
    dx150	=	a149	*	x149	-	a150	*	x150
    dx151	=	a150	*	x150	-	a151	*	x151
    dx152	=	a151	*	x151	-	a152	*	x152
    dx153	=	a152	*	x152	-	a153	*	x153
    dx154	=	a153	*	x153	-	a154	*	x154
    dx155	=	a154	*	x154	-	a155	*	x155
    dx156	=	a155	*	x155	-	a156	*	x156
    dx157	=	a156	*	x156	-	a157	*	x157
    dx158	=	a157	*	x157	-	a158	*	x158
    dx159	=	a158	*	x158	-	a159	*	x159
    dx160	=	a159	*	x159	-	a160	*	x160
    dx161	=	a160	*	x160	-	a161	*	x161
    dx162	=	a161	*	x161	-	a162	*	x162
    dx163	=	a162	*	x162	-	a163	*	x163
    dx164	=	a163	*	x163	-	a164	*	x164
    dx165	=	a164	*	x164	-	a165	*	x165
    dx166	=	a165	*	x165	-	a166	*	x166
    dx167	=	a166	*	x166	-	a167	*	x167
    dx168	=	a167	*	x167	-	a168	*	x168
    dx169	=	a168	*	x168	-	a169	*	x169
    dx170	=	a169	*	x169	-	a170	*	x170
    dx171	=	a170	*	x170	-	a171	*	x171
    dx172	=	a171	*	x171	-	a172	*	x172
    dx173	=	a172	*	x172	-	a173	*	x173
    dx174	=	a173	*	x173	-	a174	*	x174
    dx175	=	a174	*	x174	-	a175	*	x175
    dx176	=	a175	*	x175	-	a176	*	x176
    dx177	=	a176	*	x176	-	a177	*	x177
    dx178	=	a177	*	x177	-	a178	*	x178
    dx179	=	a178	*	x178	-	a179	*	x179
    dx180	=	a179	*	x179	-	a180	*	x180
    dx181	=	a180	*	x180	-	a181	*	x181
    dx182	=	a181	*	x181	-	a182	*	x182
    dx183	=	a182	*	x182	-	a183	*	x183
    dx184	=	a183	*	x183	-	a184	*	x184
    dx185	=	a184	*	x184	-	a185	*	x185
    dx186	=	a185	*	x185	-	a186	*	x186
    dx187	=	a186	*	x186	-	a187	*	x187
    dx188	=	a187	*	x187	-	a188	*	x188
    dx189	=	a188	*	x188	-	a189	*	x189
    dx190	=	a189	*	x189	-	a190	*	x190
    dx191	=	a190	*	x190	-	a191	*	x191
    dx192	=	a191	*	x191	-	a192	*	x192
    dx193	=	a192	*	x192	-	a193	*	x193
    dx194	=	a193	*	x193	-	a194	*	x194
    dx195	=	a194	*	x194	-	a195	*	x195
    dx196	=	a195	*	x195	-	a196	*	x196
    dx197	=	a196	*	x196	-	a197	*	x197
    dx198	=	a197	*	x197	-	a198	*	x198
    dx199	=	a198	*	x198	-	a199	*	x199
    dx200	=	a199	*	x199	-	a200	*	x200
    
    return(list(c(dx1  ,
dx2  ,
dx3	,
dx4	,
dx5	,
dx6	,
dx7	,
dx8	,
dx9	,
dx10	,
dx11	,
dx12	,
dx13	,
dx14	,
dx15	,
dx16	,
dx17	,
dx18	,
dx19	,
dx20	,
dx21	,
dx22	,
dx23	,
dx24	,
dx25	,
dx26	,
dx27	,
dx28	,
dx29	,
dx30	,
dx31	,
dx32	,
dx33	,
dx34	,
dx35	,
dx36	,
dx37	,
dx38	,
dx39	,
dx40	,
dx41	,
dx42	,
dx43	,
dx44	,
dx45	,
dx46	,
dx47	,
dx48	,
dx49	,
dx50	,
dx51	,
dx52	,
dx53	,
dx54	,
dx55	,
dx56	,
dx57	,
dx58	,
dx59	,
dx60	,
dx61	,
dx62	,
dx63	,
dx64	,
dx65	,
dx66	,
dx67	,
dx68	,
dx69	,
dx70	,
dx71	,
dx72	,
dx73	,
dx74	,
dx75	,
dx76	,
dx77	,
dx78	,
dx79	,
dx80	,
dx81	,
dx82	,
dx83	,
dx84	,
dx85	,
dx86	,
dx87	,
dx88	,
dx89	,
dx90	,
dx91	,
dx92	,
dx93	,
dx94	,
dx95	,
dx96	,
dx97	,
dx98	,
dx99	,
dx100	,
dx101	,
dx102	,
dx103	,
dx104	,
dx105	,
dx106	,
dx107	,
dx108	,
dx109	,
dx110	,
dx111	,
dx112	,
dx113	,
dx114	,
dx115	,
dx116	,
dx117	,
dx118	,
dx119	,
dx120	,
dx121	,
dx122	,
dx123	,
dx124	,
dx125	,
dx126	,
dx127	,
dx128	,
dx129	,
dx130	,
dx131	,
dx132	,
dx133	,
dx134	,
dx135	,
dx136	,
dx137	,
dx138	,
dx139	,
dx140	,
dx141	,
dx142	,
dx143	,
dx144	,
dx145	,
dx146	,
dx147	,
dx148	,
dx149	,
dx150	,
dx151	,
dx152	,
dx153	,
dx154	,
dx155	,
dx156	,
dx157	,
dx158	,
dx159	,
dx160	,
dx161	,
dx162	,
dx163	,
dx164	,
dx165	,
dx166	,
dx167	,
dx168	,
dx169	,
dx170	,
dx171	,
dx172	,
dx173	,
dx174	,
dx175	,
dx176	,
dx177	,
dx178	,
dx179	,
dx180	,
dx181	,
dx182	,
dx183	,
dx184	,
dx185	,
dx186	,
dx187	,
dx188	,
dx189	,
dx190	,
dx191	,
dx192	,
dx193	,
dx194	,
dx195	,
dx196	,
dx197	,
dx198	,
dx199	,
dx200)))
  })
}

library(deSolve)
zero = c(x = vector(length = 200)*0)
zero[1] = 100
yini = zero
times = seq(0,500,0.5)

#define reaction rates, follow a poisson distribution
#rand = rlnorm(200,0,1)
A = vector(length = 200)+1#seq(1.02,5, 0.02)
pars  = c(a = A)

out   = ode(func = lemaire, y = yini, parms = pars, times = times)
