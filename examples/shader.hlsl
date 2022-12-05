cbuffer Matrices : register(b0)
{
	matrix projection;
	matrix view;
};

cbuffer Model : register(b1)
{
	matrix model;
};

struct VertexOut
{
	float4 position : SV_POSITION;
	float4 color : COLOR;
};

VertexOut VShader(float4 position : POSITION, float4 color : COLOR)
{
	VertexOut output;
	output.position = mul(mul(mul(projection, view), model), position);
	output.color = color;

	return output;
}

float4 PShader(float4 position : SV_POSITION, float4 color : COLOR) : SV_TARGET
{
	return color;
}

StructuredBuffer<uint> bufferIn : register(t0);
RWBuffer<float> bufferOut : register(u0);

[numthreads(1, 1, 1)]
void CShader(uint3 threadId : SV_DispatchThreadID)
{
	uint shiftAmount = (threadId.x & 3) << 3;
	uint byte = (bufferIn[threadId.x / 4]) >> shiftAmount & 0xFF;
	bufferOut[threadId.x] = byte * 42;

	for (int z = 0; z < 2; ++z)
	for (int y = 0; y < 2; ++y)
	for (int x = 0; x < 2; ++x)
	{
		int vertexIdx = z * 4 + y * 2 + x;
		bufferOut[vertexIdx * 7 + 0] = 0.5 - x;
		bufferOut[vertexIdx * 7 + 1] = 0.5 - y;
		bufferOut[vertexIdx * 7 + 2] = 0.5 - z;

		bufferOut[vertexIdx * 7 + 3] = x;
		bufferOut[vertexIdx * 7 + 4] = y;
		bufferOut[vertexIdx * 7 + 5] = z;
		bufferOut[vertexIdx * 7 + 6] = 1;
	}
}
